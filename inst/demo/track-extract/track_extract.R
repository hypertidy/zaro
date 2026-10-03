# track_extract.R -- point/track extraction from the BRAN2023 virtual Zarr
#
# The store is a VirtualiZarr Kerchunk-Parquet reference set on GitHub
# (mdsumner/virtualized, remote/*.parq) whose chunk refs point at byte
# ranges inside NetCDF files on the NCI THREDDS fileServer.
#
# Layers:
#   zaro     opens the store (.zmetadata, Parquet manifest shards), gives
#            the array metadata and the codec pipeline (shuffle + zlib)
#   altarr   presents temp as a lazy R array; x[cbind(i, j, k, l)] plans the
#            chunks the points need and calls fetch() once with all of them
#   here     a batched fetch: resolve every chunk to (url, offset, size)
#            from the manifest, then issue the byte-range GETs through one
#            curl multi pool capped at `max_con` connections (default 8),
#            then decode with zaro
#
# zaro_array() would work as is, but it fetches the batch one chunk at a
# time (or via future.apply worker processes). The capped curl pool is what
# a polite-to-THREDDS run wants, so the fetch is built here from zaro's
# internals. ASCII only.

suppressPackageStartupMessages({
  library(zaro)
  library(altarr)
  library(curl)
})

zi <- function(name) getFromNamespace(name, "zaro")

BRAN_REFS <- "https://raw.githubusercontent.com/mdsumner/virtualized/refs/heads/main/remote"

# zaro reads manifest shards with arrow::read_parquet(). Where arrow is not
# installed, read them with nanoparquet instead (same columns).
if (!requireNamespace("arrow", quietly = TRUE) &&
    requireNamespace("nanoparquet", quietly = TRUE)) {
  local({
    shard_np <- function(store, var_name, shard_idx) {
      full <- paste0(store@root, "/", var_name, "/refs.", shard_idx, ".parq")
      pq <- getFromNamespace("vz_fetch_parquet_raw", "zaro")(full)
      if (is.null(pq)) return(NULL)
      tbl <- nanoparquet::read_parquet(pq)
      shard <- data.frame(path = as.character(tbl$path),
                          offset = as.numeric(tbl$offset),
                          size = as.integer(tbl$size),
                          stringsAsFactors = FALSE)
      if ("raw" %in% names(tbl)) shard$raw <- as.list(tbl$raw)
      shard
    }
    utils::assignInNamespace("vz_fetch_shard", shard_np, "zaro")
  })
}

#' Open one BRAN2023 variable store, e.g. "ocean_temp_2023"
open_bran <- function(name = "ocean_temp_2023", refs = BRAN_REFS,
                      verbose = FALSE) {
  zaro(paste0("virtualizarr://", refs, "/", name, ".parq"), verbose = verbose)
}

#' Read a 1-D coordinate array (inlined in the manifest, no THREDDS access)
bran_coord <- function(store, var) {
  # unlist(): zaro_read() returns a list-array when fill_value is null
  as.numeric(unlist(zaro_read(store, var, verbose = FALSE)))
}

#' Resolve Zarr chunk indices (0-based, Zarr/C order, one row per chunk) to
#' byte references. Manifest shards are fetched from GitHub once and cached
#' in the store.
vz_resolve <- function(store, var, zidx, meta) {
  grid <- ceiling(meta@shape / meta@chunk_shape)
  stride <- rev(cumprod(rev(c(grid[-1], 1))))
  lin <- as.numeric(zidx %*% stride)
  cache <- store@cache
  ss <- cache$shard_sizes[[var]]
  if (is.null(ss)) {
    s0 <- zi("vz_fetch_shard")(store, var, 0L)
    ss <- nrow(s0)
    cache$shard_sizes[[var]] <- ss
    cache$shards[[paste0(var, ":0")]] <- s0
  }
  sh <- lin %/% ss
  row <- lin %% ss + 1
  out <- data.frame(path = rep(NA_character_, length(lin)),
                    offset = NA_real_, size = NA_integer_)
  for (s in unique(sh)) {
    key <- paste0(var, ":", s)
    tab <- cache$shards[[key]]
    if (is.null(tab)) {
      tab <- zi("vz_fetch_shard")(store, var, as.integer(s))
      cache$shards[[key]] <- tab
    }
    w <- which(sh == s)
    out$path[w] <- tab$path[row[w]]
    out$offset[w] <- tab$offset[row[w]]
    out$size[w] <- tab$size[row[w]]
  }
  out
}

# transport counters, one environment per session
.track_stats <- new.env()
track_stats_reset <- function() {
  .track_stats$requests <- 0L
  .track_stats$bytes <- 0
  .track_stats$batches <- 0L
  .track_stats$seconds <- 0
  invisible(NULL)
}
track_stats_reset()
track_stats <- function() as.list(.track_stats)

#' Byte-range GET of many refs through one curl pool, at most `max_con`
#' connections open at once (total and per host). Returns a list of raw.
range_get_many <- function(refs, max_con = 8L,
                           url_hook = function(url, k) url,
                           retries = 2L) {
  n <- nrow(refs)
  res <- vector("list", n)
  t0 <- Sys.time()
  todo <- seq_len(n)
  for (attempt in 0:retries) {
    if (!length(todo)) break
    pool <- new_pool(total_con = max_con, host_con = max_con, multiplex = FALSE)
    failed <- integer(0)
    for (i in todo) {
      local({
        k <- i
        rng <- sprintf("bytes=%.0f-%.0f", refs$offset[k],
                       refs$offset[k] + refs$size[k] - 1)
        h <- new_handle()
        handle_setheaders(h, Range = rng)
        curl_fetch_multi(url_hook(refs$path[k], k), handle = h, pool = pool,
          done = function(r) {
            if (r$status_code %in% c(200L, 206L) &&
                length(r$content) == refs$size[k]) {
              res[[k]] <<- r$content
            } else {
              failed <<- c(failed, k)
            }
          },
          fail = function(msg) failed <<- c(failed, k))
      })
    }
    .track_stats$requests <- .track_stats$requests + length(todo)
    multi_run(pool = pool)
    todo <- failed
  }
  if (length(todo)) {
    stop(length(todo), " byte-range requests failed after retries",
         call. = FALSE)
  }
  .track_stats$bytes <- .track_stats$bytes + sum(refs$size)
  .track_stats$batches <- .track_stats$batches + 1L
  .track_stats$seconds <- .track_stats$seconds +
    as.numeric(difftime(Sys.time(), t0, units = "secs"))
  res
}

#' A lazy array over one variable, batched and connection-capped.
#' R dim order is the Zarr shape reversed: for temp that is
#' (xt_ocean, yt_ocean, st_ocean, Time).
bran_array <- function(store, var = "temp", max_con = 8L,
                       url_hook = function(url, zidx) url) {
  meta <- zi("resolve_array_meta")(store, var, NULL, FALSE)
  spec <- zi("zaro_array_spec")(meta)
  stopifnot(spec$rev_c)
  cs <- spec$chunk
  d <- spec$dim
  cast <- switch(spec$type, logical = as.logical, integer = as.integer,
                 as.double)
  fill <- suppressWarnings(
    cast(zi("coerce_fill_value")(meta@fill_value, meta@data_type))[1L])
  decode <- zi("decode_chunk")
  clip <- zi("clip_chunk")
  fetch <- function(chunks) {
    chunks <- matrix(as.integer(chunks), ncol = length(d))
    zidx <- chunks[, rev(seq_len(ncol(chunks))), drop = FALSE]
    refs <- vz_resolve(store, var, zidx, meta)
    ok <- !is.na(refs$path) & nzchar(refs$path)
    raw <- vector("list", nrow(refs))
    if (any(ok)) {
      raw[ok] <- range_get_many(refs[ok, , drop = FALSE], max_con,
        url_hook = function(u, k) url_hook(u, zidx[which(ok)[k], ]))
    }
    lapply(seq_len(nrow(chunks)), function(r) {
      st <- chunks[r, ] * cs + 1L
      ext <- as.integer(pmin(cs, d - st + 1L))
      if (is.null(raw[[r]])) return(rep(fill, prod(ext)))
      clip(cast(decode(raw[[r]], meta)), cs, ext)
    })
  }
  altarr(spec$dim, cs, fetch, dimnames = spec$dimnames, type = spec$type)
}

#' Nearest index of each value in a sorted coordinate vector
nearest <- function(coord, v) {
  i <- findInterval(v, coord, all.inside = TRUE)
  i + (abs(coord[i + 1L] - v) < abs(v - coord[i]))
}

#' Map track points to R-order array indices (x, y, z, t).
#' lon in either -180..180 or 0..360; depth in metres; time as POSIXct/Date.
track_index <- function(coords, lon, lat, depth, time) {
  lon <- lon %% 360
  tday <- as.numeric(difftime(as.POSIXct(time, tz = "UTC"),
                              as.POSIXct("1979-01-01", tz = "UTC"),
                              units = "days"))
  cbind(x = nearest(coords$xt_ocean, lon),
        y = nearest(coords$yt_ocean, lat),
        z = nearest(coords$st_ocean, depth),
        t = nearest(coords$Time, tday))
}

#' Extract values at track points: one batched fetch for the whole track.
#' Applies CF packing (scale_factor, add_offset, missing values to NA).
track_extract <- function(x, idx, attrs) {
  v <- x[idx]
  miss <- c(attrs$missing_value, attrs[["_FillValue"]], -32767)
  v[v %in% miss] <- NA
  v * (attrs$scale_factor %||% 1) + (attrs$add_offset %||% 0)
}

bran_attrs <- function(store, var) {
  zm <- jsonlite::fromJSON(zi("sanitize_json")(rawToChar(store_get_raw(store, ".zmetadata"))),
                           simplifyVector = TRUE)
  zm$metadata[[paste0(var, "/.zattrs")]]
}
store_get_raw <- function(store, key) zi("store_get")(store, key)

`%||%` <- function(a, b) if (is.null(a)) b else a
