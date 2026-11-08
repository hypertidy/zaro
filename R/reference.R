# reference.R - Kerchunk JSON and Parquet virtual reference stores
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

  txt <- sanitize_json(txt)
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
  file <- vz_fetch_parquet_raw(source)
  if (is.null(file)) stop("cannot read Parquet reference file: ", source, call. = FALSE)
  if (!identical(file, source)) on.exit(unlink(file), add = TRUE)
  tbl <- read_refs_parquet(file)

  # normalise column names - different tools use different conventions
  nms <- tolower(names(tbl))
  names(tbl) <- nms

  key_col <- intersect(c("key", "path", "name"), nms)[1]
  url_col <- intersect(c("url", "uri", "path", "file"), nms)
  url_col <- setdiff(url_col, key_col)[1]
  offset_col <- intersect(c("offset", "start", "byte_offset"), nms)[1]
  length_col <- intersect(c("length", "size", "byte_length"), nms)[1]

  if (is.na(key_col)) stop("cannot identify key column in parquet reference", call. = FALSE)

  # per row: a byte reference when the row has a url, else inline data
  # (raw bytes or text, e.g. metadata rows)
  n <- nrow(tbl)
  keys <- as.character(tbl[[key_col]])
  is_ref <- if (!is.na(url_col) && !is.na(offset_col) && !is.na(length_col)) {
    u <- tbl[[url_col]]
    !is.na(u) & nzchar(u)
  } else rep(FALSE, n)
  refs <- vector("list", n)
  if (any(is_ref)) {
    refs[is_ref] <- Map(function(u, o, l) list(u, o, l),
                        as.character(tbl[[url_col]][is_ref]),
                        as.numeric(tbl[[offset_col]][is_ref]),
                        as.numeric(tbl[[length_col]][is_ref]))
  }
  data_col <- setdiff(nms, c(key_col, url_col, offset_col, length_col))
  if (any(!is_ref) && length(data_col) > 0) {
    d <- tbl[[data_col[1]]]
    refs[!is_ref] <- if (is.list(d)) d[!is_ref] else as.list(as.character(d[!is_ref]))
  }
  keep <- !vapply(refs, is.null, logical(1))
  refs <- stats::setNames(refs[keep], keys[keep])

  ReferenceStore(
    root = "",
    refs = refs,
    templates = list()
  )
}


#' Read a Parquet reference table (manifest) into a data frame
#'
#' nanoparquet reads the whole file, which suits reference manifests:
#' VirtualiZarr and kerchunk shard them (e.g. 100k rows a file), and only
#' the shards a selection needs are fetched. Binary columns become a plain
#' list of raw vectors (NULL where missing); 64-bit integers become double
#' (exact to 2^53).
#' @noRd
read_refs_parquet <- function(file) {
  tbl <- nanoparquet::read_parquet(file)
  for (nm in names(tbl)) {
    if (inherits(tbl[[nm]], "blob")) tbl[[nm]] <- unclass(tbl[[nm]])
  }
  as.data.frame(tbl, stringsAsFactors = FALSE)
}


#' Read text from a file path or URL
#' @noRd
readr_or_readlines <- function(source) {
  if (grepl("^https?://", source)) {
    # try gdalraster VSI first for HTTP
    if (requireNamespace("gdalraster", quietly = TRUE)) {
      vsi_path <- paste0("/vsicurl/", source)
      tryCatch({
        con <- methods::new(gdalraster::VSIFile, vsi_path, "rb")
        raw <- tryCatch(con$ingest(-1), finally = con$close())
        return(rawToChar(raw))
      }, error = function(e) NULL)
    }
    # fallback to base R
    return(paste(readLines(url(source), warn = FALSE), collapse = "\n"))
  }
  paste(readLines(source, warn = FALSE), collapse = "\n")
}
