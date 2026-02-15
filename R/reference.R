# reference.R — Kerchunk JSON and Parquet virtual reference stores
#
# These allow Zarr-style access to existing archival files (NetCDF, HDF5,
# GRIB, TIFF) without reformatting. A reference document maps Zarr keys
# to (url, offset, length) tuples pointing into the original files.
#
# JSON reference stores come in two versions:
#   V0: {"key": [url, offset, length], ...}
#   V1: {"version": 1, "refs": {...}, "templates": {...}}
#
# Parquet reference stores encode the same mapping in a columnar format
# for large reference sets (millions of chunks).

#' Parse a Kerchunk JSON reference document
#'
#' @param source path or URL to the JSON reference file, or a raw vector
#' @returns ReferenceStore object
#' @noRd
parse_kerchunk_json <- function(source) {
  if (is.raw(source)) {
    txt <- rawToChar(source)
  } else if (is.character(source)) {
    txt <- readr_or_readlines(source)
  } else {
    stop("source must be a file path, URL, or raw vector", call. = FALSE)
  }

  doc <- jsonlite::fromJSON(txt, simplifyVector = FALSE)

  version <- doc[["version"]] %||% 0L
  templates <- list()

  if (version >= 1L) {
    refs <- doc[["refs"]] %||% list()
    templates <- doc[["templates"]] %||% list()
  } else {
    # V0: the entire document is the refs
    refs <- doc
    # remove any non-ref metadata keys
    refs[["version"]] <- NULL
  }

  ReferenceStore(
    root = "",
    refs = refs,
    templates = templates
  )
}


#' Parse a Kerchunk Parquet reference store
#'
#' Parquet reference stores contain columns: key (or path), url (or path),
#' offset, length. The exact column naming varies by producer.
#'
#' @param source path or URL to the Parquet file
#' @returns ReferenceStore object
#' @noRd
parse_kerchunk_parquet <- function(source) {
  tbl <- arrow::read_parquet(source)

  # normalise column names — different tools use different conventions
  nms <- tolower(names(tbl))
  names(tbl) <- nms

  key_col <- intersect(c("key", "path", "name"), nms)[1]
  url_col <- intersect(c("url", "uri", "path", "file"), nms)
  url_col <- setdiff(url_col, key_col)[1]
  offset_col <- intersect(c("offset", "start", "byte_offset"), nms)[1]
  length_col <- intersect(c("length", "size", "byte_length"), nms)[1]

  if (is.na(key_col)) stop("cannot identify key column in parquet reference", call. = FALSE)

  refs <- list()
  for (i in seq_len(nrow(tbl))) {
    k <- tbl[[key_col]][i]
    if (!is.na(url_col) && !is.na(offset_col) && !is.na(length_col)) {
      refs[[k]] <- list(tbl[[url_col]][i],
                        tbl[[offset_col]][i],
                        tbl[[length_col]][i])
    } else {
      # inline data (metadata rows may be stored as raw or character)
      data_col <- setdiff(nms, c(key_col, url_col, offset_col, length_col))
      if (length(data_col) > 0) {
        refs[[k]] <- as.character(tbl[[data_col[1]]][i])
      }
    }
  }

  ReferenceStore(
    root = "",
    refs = refs,
    templates = list()
  )
}


#' Read text from a file path or URL
#' @noRd
readr_or_readlines <- function(source) {
  if (grepl("^https?://", source)) {
    # try gdalraster VSI first for HTTP
    if (requireNamespace("gdalraster", quietly = TRUE)) {
      vsi_path <- paste0("/vsicurl/", source)
      tryCatch({
        con <- gdalraster::vsi_open(vsi_path, "rb")
        on.exit(gdalraster::vsi_close(con))
        sz <- gdalraster::vsi_stat(vsi_path, "size")
        raw <- gdalraster::vsi_read(con, sz)
        return(rawToChar(raw))
      }, error = function(e) NULL)
    }
    # fallback to base R
    return(paste(readLines(url(source), warn = FALSE), collapse = "\n"))
  }
  paste(readLines(source, warn = FALSE), collapse = "\n")
}
