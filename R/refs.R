# refs.R -- the refs table: where a key's bytes are, before fetching them
#
# Every store answers store_refs(store, keys) with one row per key:
#
#   key        the Zarr key asked for
#   path       where the bytes are (object URL, file path, or VSI path)
#   offset     byte offset within path (0 for a whole object; negative for
#              the last `size` bytes, as for a shard index at the end)
#   size       bytes to read; NA when the whole object is wanted and its
#              size is not known without a request (native Zarr chunks)
#   transport  how to get them:
#                store   the store's own get (Arrow S3/GCS with its
#                        credentials, or any store without a refs method)
#                http    plain HTTP(S) GET, ranged when length is known
#                s3, gs  an object URL, read with Arrow's default filesystem
#                vsi     a GDAL virtual file path (gdalraster)
#                local   a local file
#                inline  the bytes are in the table (`data` column)
#                absent  no such chunk: reads as fill_value
#   data       list column of raw vectors (NULL unless transport "inline")
#
# Column names follow the byterefs view (https://github.com/byterefs/spec):
# path, offset, size, data. zaro_plan() adds present and dim_0..dim_{n-1}.
#
# Native, virtual and (later) sharded stores all reduce to this, so one
# fetcher and one decoder serve every backend.


#' Build a refs table
#' @noRd
refs_frame <- function(keys, path = NA_character_, offset = NA_real_,
                       size = NA_real_, transport = "store") {
  n <- length(keys)
  out <- data.frame(key = as.character(keys),
                    path = rep_len(as.character(path), n),
                    offset = rep_len(as.numeric(offset), n),
                    size = rep_len(as.numeric(size), n),
                    transport = rep_len(as.character(transport), n),
                    stringsAsFactors = FALSE)
  out$data <- vector("list", n)
  out
}


#' The transport for a URL or path found in a reference
#' @noRd
url_transport <- function(url) {
  ifelse(grepl("^https?://", url), "http",
  ifelse(grepl("^s3://", url), "s3",
  ifelse(grepl("^gs://", url), "gs",
  ifelse(grepl("^/vsi", url), "vsi", "local"))))
}


#' Fetch the bytes for one refs-table row
#'
#' Returns a raw vector, or NULL for an absent chunk (or a whole-object read
#' that finds nothing).
#' @noRd
fetch_ref <- function(store, ref) {
  stopifnot(nrow(ref) == 1L)
  switch(ref$transport,
    absent = NULL,
    inline = ref$data[[1L]],
    store  = if (is.na(ref$size)) store_get(store, ref$key) else
               store_range(store, ref$key, ref$offset, ref$size),
    {
      if (is.na(ref$size)) {
        read_whole(ref$path, ref$transport)
      } else {
        byte_range_read(ref$path, ref$offset, ref$size)
      }
    }
  )
}


#' Read a whole object or file, NULL when it does not exist
#' @noRd
read_whole <- function(url, transport = url_transport(url)) {
  switch(transport,
    http  = vz_http_get(url),
    local = if (file.exists(url)) readBin(url, "raw", file.size(url)) else NULL,
    s3 = , gs = if (!has_arrow()) read_whole(cloud_to_vsi(url), "vsi") else tryCatch({
      fp <- arrow::FileSystem$from_uri(url)
      f <- fp$fs$OpenInputFile(fp$path)
      on.exit(f$close())
      as.raw(f$Read())
    }, error = function(e) NULL),
    vsi = tryCatch({
      vsi <- methods::new(gdalraster::VSIFile, url, "rb")
      tryCatch(vsi$ingest(-1), finally = vsi$close())
    }, error = function(e) NULL),
    stop("unknown transport '", transport, "'", call. = FALSE)
  )
}
