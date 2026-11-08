# plan.R -- zaro_plan(): the refs table for a selection, before any read
#
# address --resolve--> store --plan(selection)--> refs table --fetch--> raw
#   --decode--> array
#
# zaro_plan() is the "plan" step made visible: which chunks a selection
# needs, where each one's bytes are, and what reading them will cost.


#' Plan a read: the byte references a selection needs, without reading them
#'
#' Works out which chunks of an array a hyperslab touches and where each
#' chunk's bytes are: a whole object in a native store, a byte range inside
#' a NetCDF/HDF5/TIFF file for a virtual store (kerchunk, VirtualiZarr,
#' mdim-refs), inline bytes, or absent (reads as `fill_value`). No chunk data
#' is read. For a native store no request is made at all; a virtual store
#' may fetch the manifest it needs (cached in the store).
#'
#' The result is a data frame you can inspect, filter, save or hand to
#' another tool. Printing it summarises the cost: requests, known bytes and
#' hosts.
#'
#' @param store a store from [zaro()], or a [zaro_dataset()].
#' @param path character. Path to the array within the store.
#' @param start,count the selection, as for [zaro_read()]: 0-based start and
#'   element counts in Zarr dimension order, `NA` count for the rest of an
#'   axis. Default the whole array.
#' @param meta optional array metadata, as for [zaro_read()].
#' @param chunks optional integer matrix of 0-based chunk grid indices (one
#'   row per chunk, Zarr dimension order), instead of `start`/`count`.
#' @param verbose logical. Emit diagnostic messages. Default
#'   `getOption("zaro.verbose", TRUE)`.
#' @returns A `zaro_plan` data frame, one row per chunk: `key`, `url`,
#'   `offset`, `length` (`NA` for a whole object of unknown size), `transport`
#'   (`"store"`, `"http"`, `"s3"`, `"gs"`, `"vsi"`, `"local"`, `"inline"` or
#'   `"absent"`), `inline` (list of raw) and `index` (list of the chunk's
#'   0-based grid index). Attributes `path` and `meta` record what was
#'   planned.
#'
#' @export
#' @examples
#' \dontrun{
#' store <- zaro("virtualizarr://https://raw.githubusercontent.com/mdsumner/virtualized/refs/heads/main/remote/ocean_temp_2023.parq")
#' p <- zaro_plan(store, "temp", start = c(0, 0, 0, 0), count = c(1, 1, NA, NA))
#' p                          # 60 byte ranges on one host, ~4 MB
#' table(p$transport)
#' }
zaro_plan <- function(store, path = "", start = NULL, count = NULL,
                      meta = NULL, chunks = NULL, verbose = zaro_verbose()) {
  if (path == ".") path <- ""
  if (inherits(store, "zaro_dataset")) {
    if (is.null(meta)) meta <- store$arrays[[path]]
    store <- store$store
  }
  meta <- resolve_array_meta(store, path, meta, verbose)

  if (is.null(chunks)) {
    sel <- resolve_selection(meta, start, count)
    ranges <- chunk_coverage(sel$start, sel$count, meta@chunk_shape)
    chunks <- as.matrix(expand.grid(ranges, KEEP.OUT.ATTRS = FALSE))
  }
  chunks <- matrix(as.integer(chunks), ncol = length(meta@shape))
  grid <- ceiling(meta@shape / meta@chunk_shape)
  if (any(chunks < 0L) || any(sweep(chunks, 2L, grid, `>=`))) {
    stop("chunk index outside the chunk grid (", paste(grid, collapse = " x "),
         ")", call. = FALSE)
  }

  ck <- apply(chunks, 1L, function(i) chunk_key(meta, i))
  keys <- if (nzchar(path)) paste0(path, "/", ck) else ck
  plan <- store_refs(store, keys)
  plan$index <- lapply(seq_len(nrow(chunks)), function(r) chunks[r, ])
  vmsg("planned ", nrow(plan), " chunk(s) for path '", path, "'",
       verbose = verbose)
  structure(plan, class = c("zaro_plan", "data.frame"), path = path,
            meta = meta)
}


#' @export
print.zaro_plan <- function(x, ...) {
  s <- plan_summary(x)
  cat(sprintf("<zaro_plan> '%s': %d chunk(s), %d request(s)\n",
              attr(x, "path"), s$chunks, s$requests))
  by <- paste(sprintf("%s %d", names(s$transport), s$transport), collapse = ", ")
  cat("  transport: ", by, "\n", sep = "")
  if (s$requests) {
    known <- s$requests - s$unknown
    parts <- c(if (known) format_bytes(s$bytes),
               if (s$unknown) sprintf("%d whole object(s) of unknown size",
                                      s$unknown))
    cat("  bytes:     ", paste(parts, collapse = " + "), "\n", sep = "")
    if (length(s$hosts)) {
      cat("  hosts:     ", paste(s$hosts, collapse = ", "), "\n", sep = "")
    }
  }
  show <- utils::head(as.data.frame(x)[c("key", "transport", "offset",
                                         "length", "url")], 6L)
  long <- !is.na(show$url) & nchar(show$url) > 36L
  show$url[long] <- paste0("...", substring(show$url[long],
                                            nchar(show$url[long]) - 32L))
  print(show, row.names = FALSE)
  if (nrow(x) > 6L) cat("  ... ", nrow(x) - 6L, " more\n", sep = "")
  invisible(x)
}


#' Requests, bytes and hosts of a plan
#' @noRd
plan_summary <- function(plan) {
  fetch <- !plan$transport %in% c("inline", "absent")
  len <- plan$length[fetch]
  url <- plan$url[fetch]
  host <- ifelse(grepl("^[a-z0-9+]+://", url),
                 sub("^[a-z0-9+]+://([^/]+).*$", "\\1", url), NA_character_)
  list(chunks = nrow(plan), requests = sum(fetch),
       bytes = sum(len, na.rm = TRUE), unknown = sum(is.na(len)),
       transport = table(factor(plan$transport,
                                levels = unique(plan$transport))),
       hosts = sort(unique(stats::na.omit(host))))
}


#' Resolve 0-based start/count (NA count = rest of axis), clamped to the array
#' @noRd
resolve_selection <- function(meta, start = NULL, count = NULL) {
  ndim <- length(meta@shape)
  if (is.null(start)) start <- rep(0L, ndim)
  if (is.null(count)) count <- rep(NA_integer_, ndim)
  start <- as.integer(start)
  count <- as.integer(count)
  if (length(start) != ndim || length(count) != ndim) {
    stop("start and count must have length ", ndim, call. = FALSE)
  }
  na_idx <- is.na(count)
  count[na_idx] <- meta@shape[na_idx] - start[na_idx]
  if (any(start + count > meta@shape)) {
    over <- which(start + count > meta@shape)
    warning("request exceeds array extent on dimension(s) ",
            paste(over, collapse = ", "), "; clamping", call. = FALSE)
    count <- pmin(count, meta@shape - start)
  }
  list(start = start, count = count)
}
