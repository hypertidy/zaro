# fetch.R -- zaro_fetch(): execute a plan, HTTP through one curl pool
#
# plan (refs table) --fetch--> list of raw (NULL for absent chunks)
#
# HTTP rows (native chunks of an HTTP store, byte ranges of a virtual
# store) all go through one curl multi pool:
#   - at most `max_con` connections at once, per host and in total
#   - ranged GETs when the length is known, whole-object GETs otherwise
#   - byte ranges on the same URL that touch (or are within `merge_gap`
#     bytes of each other) are merged into one request, then split
#   - failed or short responses are retried with backoff
# S3 and GCS rows join the pool as HTTPS when their access is known
# (anonymous, or AWS keys in the environment, signed by curl's SigV4; see
# cloud.R). Other transports (S3/GCS with profile or role credentials,
# local files, VSI, inline, absent) are fetched row by row with fetch_ref().


#' Fetch the bytes of a plan
#'
#' Executes a [zaro_plan()]: returns the raw (still encoded) bytes of every
#' chunk, in plan order, `NULL` for chunks that are absent (they read as
#' `fill_value`). HTTP references are fetched concurrently through one curl
#' connection pool with a per-host cap, adjacent byte ranges are merged into
#' single requests, and failures are retried. S3 and GCS objects join the
#' same pool as HTTPS when their access is known: anonymous (`zaro(...,
#' anonymous = TRUE)`, a known public bucket, `AWS_NO_SIGN_REQUEST=YES` or
#' `options(zaro.cloud_anonymous = TRUE)`), or S3 signed with
#' `AWS_ACCESS_KEY_ID` / `AWS_SECRET_ACCESS_KEY` (and `AWS_SESSION_TOKEN`)
#' from the environment. Other credentials (profiles, instance roles,
#' signed GCS) are read through Arrow or GDAL one object at a time. [zaro_read()] and
#' [zaro_array()] read through this, so they get the same batching.
#'
#' @param store the store the plan was made from.
#' @param plan a `zaro_plan` (or any refs table with the same columns).
#' @param max_con integer. Maximum simultaneous HTTP connections, per host
#'   and in total. Default `getOption("zaro.max_con", 8L)`.
#' @param merge_gap numeric. Merge byte ranges on the same URL separated by
#'   at most this many bytes (the gap is fetched and discarded). Default
#'   `getOption("zaro.merge_gap", 0)`: merge only touching ranges.
#' @param retries integer. Retries for failed HTTP requests.
#' @param verbose logical. Emit diagnostic messages. Default
#'   `getOption("zaro.verbose", TRUE)`.
#' @returns A list of raw vectors (or `NULL`), one per plan row, with
#'   attribute `"stats"`: HTTP `requests`, `bytes` received and `seconds`.
#'
#' @export
#' @examples
#' \dontrun{
#' p <- zaro_plan(store, "temp", start = c(0, 0, 0, 0), count = c(1, 1, NA, NA))
#' raw <- zaro_fetch(store, p)
#' attr(raw, "stats")
#' }
zaro_fetch <- function(store, plan, max_con = getOption("zaro.max_con", 8L),
                       merge_gap = getOption("zaro.merge_gap", 0),
                       retries = 2L, verbose = zaro_verbose()) {
  if (inherits(store, "zaro_dataset")) store <- store$store
  n <- nrow(plan)
  out <- vector("list", n)
  stats <- list(requests = 0L, bytes = 0, seconds = 0)

  # S3/GCS objects whose access is known go through the pool too, as HTTPS
  plan <- plan_cloud_to_http(plan, store)
  http <- which(plan$transport == "http")
  if (length(http)) {
    got <- http_fetch(plan$path[http], plan$offset[http], plan$size[http],
                      max_con = max_con, merge_gap = merge_gap,
                      retries = retries,
                      opts = attr(plan, "curl_opts")[http])
    out[http] <- got
    stats <- attr(got, "stats")
    vmsg("fetched ", length(http), " chunk(s) in ", stats$requests,
         " HTTP request(s), ", format_bytes(stats$bytes), ", ",
         sprintf("%.2f s", stats$seconds), verbose = verbose)
  }
  for (i in setdiff(seq_len(n), http)) {
    r <- fetch_ref(store, plan[i, , drop = FALSE])
    if (!is.null(r)) out[[i]] <- r
  }
  attr(out, "stats") <- stats
  out
}


#' Many HTTP GETs through one capped curl pool
#'
#' `length` NA means the whole object (no Range header; a plan's `size`); a 404 or 403 for a
#' whole object is an absent chunk (NULL), not an error.
#' @noRd
http_fetch <- function(url, offset, length, max_con = 8L, merge_gap = 0,
                       retries = 2L, max_merge = 16 * 1024^2,
                       absent_ok = is.na(length), opts = NULL) {
  t0 <- Sys.time()
  reqs <- http_requests(url, offset, length, merge_gap, max_merge)
  for (q in seq_along(reqs)) reqs[[q]]$absent_ok <- all(absent_ok[reqs[[q]]$rows])
  res <- vector("list", length(url))
  n_req <- 0L
  n_bytes <- 0
  todo <- seq_along(reqs)
  errs <- character()
  for (attempt in 0:retries) {
    if (!length(todo)) break
    if (attempt > 0L) Sys.sleep(0.5 * 2^(attempt - 1L))
    pool <- curl::new_pool(total_con = max_con, host_con = max_con,
                           multiplex = FALSE)
    failed <- integer()
    for (q in todo) {
      local({
        rq <- reqs[[q]]
        h <- curl::new_handle()
        # signing options (S3 SigV4), the same for every row of one URL
        o <- if (!is.null(opts)) opts[[rq$rows[1L]]]
        hdrs <- o$headers
        o$headers <- NULL
        if (length(o)) curl::handle_setopt(h, .list = o)
        if (isTRUE(rq$suffix)) {
          hdrs <- c(hdrs, Range = sprintf("bytes=-%.0f", rq$len))
        } else if (!is.na(rq$start)) {
          hdrs <- c(hdrs, Range = sprintf("bytes=%.0f-%.0f", rq$start, rq$end))
        }
        if (length(hdrs)) curl::handle_setheaders(h, .list = as.list(hdrs))
        # a signed 403 is a refusal, not a missing object
        absent_codes <- if (length(o)) c(404L, 416L) else c(403L, 404L, 416L)
        curl::curl_fetch_multi(rq$url, handle = h, pool = pool,
          done = function(r) {
            body <- http_body(r, rq)
            if (is.null(body)) {
              if (rq$absent_ok && r$status_code %in% absent_codes) {
                return(invisible())                  # absent object
              }
              failed <<- c(failed, q)
              errs <<- c(errs, sprintf("HTTP %d for %s", r$status_code, rq$url))
              return(invisible())
            }
            n_bytes <<- n_bytes + base::length(r$content)
            for (m in seq_along(rq$rows)) {
              res[[rq$rows[m]]] <<- if (is.na(rq$start) || isTRUE(rq$suffix)) body else
                body[seq.int(rq$rel[m] + 1, length.out = rq$len[m])]
            }
          },
          fail = function(msg) {
            failed <<- c(failed, q)
            errs <<- c(errs, paste0(msg, " for ", rq$url))
          })
      })
    }
    n_req <- n_req + length(todo)
    curl::multi_run(pool = pool)
    todo <- failed
  }
  if (length(todo)) {
    stop(length(todo), " HTTP request(s) failed after ", retries,
         " retries; last: ", utils::tail(errs, 1L), call. = FALSE)
  }
  attr(res, "stats") <- list(
    requests = n_req, bytes = n_bytes,
    seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")))
  res
}


#' The body of a response, or NULL if it is not what was asked for
#'
#' A ranged request must come back 206 with exactly the bytes asked for, or
#' 200 with the whole object (a server that ignores Range), which is cut.
#' @noRd
http_body <- function(r, rq) {
  body <- r$content
  if (isTRUE(rq$suffix)) {                     # the last rq$len bytes
    n <- base::length(body)
    if (r$status_code == 206L && n == rq$len) return(body)
    if (r$status_code == 200L && n >= rq$len) return(body[seq.int(n - rq$len + 1, n)])
    return(NULL)
  }
  if (is.na(rq$start)) {
    return(if (r$status_code == 200L) body else NULL)
  }
  want <- rq$end - rq$start + 1
  if (r$status_code == 206L && base::length(body) == want) return(body)
  if (r$status_code == 200L && base::length(body) >= rq$end + 1) {
    return(body[seq.int(rq$start + 1, rq$end + 1)])
  }
  NULL
}


#' Group byte ranges into requests: merge touching ranges on the same URL
#'
#' Returns a list of requests, each with url, start/end (NA for a whole
#' object), and the plan rows it serves with their offset (`rel`) and length
#' within the response.
#' @noRd
http_requests <- function(url, offset, length, merge_gap = 0,
                          max_merge = 16 * 1024^2) {
  reqs <- list()
  whole <- which(is.na(length))
  for (i in whole) {
    reqs[[base::length(reqs) + 1L]] <- list(url = url[i], start = NA_real_,
                                            end = NA_real_, rows = i,
                                            rel = 0, len = NA_real_)
  }
  suffix <- setdiff(which(offset < 0), whole)
  for (i in suffix) {
    reqs[[base::length(reqs) + 1L]] <- list(url = url[i], start = NA_real_,
                                            end = NA_real_, rows = i, rel = 0,
                                            len = length[i], suffix = TRUE)
  }
  ranged <- setdiff(seq_along(url), c(whole, suffix))
  for (u in unique(url[ranged])) {
    w <- ranged[url[ranged] == u]
    w <- w[order(offset[w])]
    cur <- NULL
    flush <- function() {
      if (!is.null(cur)) {
        cur$rel <- offset[cur$rows] - cur$start
        cur$len <- length[cur$rows]
        reqs[[base::length(reqs) + 1L]] <<- cur
      }
    }
    for (i in w) {
      s <- offset[i]
      e <- offset[i] + length[i] - 1
      if (!is.null(cur) && s <= cur$end + 1 + merge_gap &&
          max(e, cur$end) - cur$start + 1 <= max_merge) {
        cur$end <- max(cur$end, e)
        cur$rows <- c(cur$rows, i)
      } else {
        flush()
        cur <- list(url = u, start = s, end = e, rows = i)
      }
    }
    flush()
  }
  reqs
}
