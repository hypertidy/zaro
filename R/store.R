# store.R — key-value store backends
#
# All stores expose the same interface:
#   $get(key)    -> raw vector or NULL
#   $list(prefix) -> character vector of keys
#   $exists(key) -> logical
#
# Backends:
#   ArrowStore     — arrow::FileSystem (local, S3, GCS)
#   VSIStore       — gdalraster VSI (HTTP, Azure, any GDAL vfs)
#   ReferenceStore — Kerchunk JSON or Parquet virtual references

# -- abstract store ---------------------------------------------------------

ZaroStore <- new_class("ZaroStore", properties = list(
  root = class_character
))

store_get <- new_generic("store_get", "store")
store_list <- new_generic("store_list", "store")
store_exists <- new_generic("store_exists", "store")

# -- Arrow filesystem store -------------------------------------------------

ArrowStore <- new_class("ArrowStore", parent = ZaroStore, properties = list(
  fs = class_any  # arrow::FileSystem (can't type-check R6)
))

method(store_get, ArrowStore) <- function(store, key) {
  path <- file.path(store@root, key)
  tryCatch({
    f <- store@fs$OpenInputFile(path)
    on.exit(f$close())
    as.raw(f$Read())  # raw vector

  }, error = function(e) NULL)
}

method(store_list, ArrowStore) <- function(store, prefix = "") {
  path <- file.path(store@root, prefix)
  sel <- arrow::FileSelector$create(path, recursive = TRUE)
  infos <- store@fs$GetFileInfo(sel)
  paths <- vapply(infos, function(i) i$path, character(1))
  # return paths relative to root
  sub(paste0("^", store@root, "/?"), "", paths)
}

method(store_exists, ArrowStore) <- function(store, key) {
  path <- file.path(store@root, key)
  tryCatch({
    info <- store@fs$GetFileInfo(path)
    info$type != 0L  # 0 = NotFound
  }, error = function(e) FALSE)
}


# -- gdalraster VSI store ---------------------------------------------------

VSIStore <- new_class("VSIStore", parent = ZaroStore)

method(store_get, VSIStore) <- function(store, key) {
  if (!requireNamespace("gdalraster", quietly = TRUE)) {
    stop("gdalraster required for VSI store access", call. = FALSE)
  }
  path <- paste0(store@root, "/", key)
  tryCatch({
    con <- gdalraster::vsi_open(path, "rb")
    on.exit(gdalraster::vsi_close(con))
    sz <- gdalraster::vsi_stat(path, "size")
    gdalraster::vsi_read(con, sz)
  }, error = function(e) NULL)
}

method(store_list, VSIStore) <- function(store, prefix = "") {
  if (!requireNamespace("gdalraster", quietly = TRUE)) {
    stop("gdalraster required for VSI store access", call. = FALSE)
  }
  path <- paste0(store@root, "/", prefix)
  entries <- gdalraster::vsi_read_dir(path, recursive = TRUE)
  if (nzchar(prefix)) {
    file.path(prefix, entries)
  } else {
    entries
  }
}

method(store_exists, VSIStore) <- function(store, key) {
  if (!requireNamespace("gdalraster", quietly = TRUE)) {
    stop("gdalraster required for VSI store access", call. = FALSE)
  }
  path <- paste0(store@root, "/", key)
  sz <- gdalraster::vsi_stat(path, "size")
  !is.na(sz) && sz >= 0
}


# -- Kerchunk reference store -----------------------------------------------

ReferenceStore <- new_class("ReferenceStore", parent = ZaroStore, properties = list(
  refs = class_list,       # named list: key -> raw (inline) or c(url, offset, length)
  templates = class_list   # named list of URI templates (V1)
))

method(store_get, ReferenceStore) <- function(store, key) {
  ref <- store@refs[[key]]
  if (is.null(ref)) return(NULL)

  # inline string data (metadata entries)
  if (is.character(ref) && length(ref) == 1L) {
    return(charToRaw(ref))
  }

  # [url, offset, length] reference to a chunk in an archival file
  if (is.list(ref) || (is.character(ref) && length(ref) == 3L)) {
    url <- ref[[1]]
    offset <- as.numeric(ref[[2]])
    len <- as.numeric(ref[[3]])

    # expand template if present
    url <- expand_template(url, store@templates)

    return(byte_range_read(url, offset, len))
  }

  NULL
}

method(store_list, ReferenceStore) <- function(store, prefix = "") {
  keys <- names(store@refs)
  if (nzchar(prefix)) {
    keys <- keys[startsWith(keys, prefix)]
  }
  keys
}

method(store_exists, ReferenceStore) <- function(store, key) {
  key %in% names(store@refs)
}


# -- helpers -----------------------------------------------------------------

#' Expand Kerchunk URI templates
#' @noRd
expand_template <- function(url, templates) {
  if (length(templates) == 0L) return(url)
  for (nm in names(templates)) {
    url <- gsub(paste0("{{", nm, "}}"), templates[[nm]], url, fixed = TRUE)
  }
  url
}

#' Read a byte range from a URL or file path
#'
#' Dispatches to the best available method: arrow S3/GCS for cloud URIs,
#' gdalraster VSI for everything else.
#' @noRd
byte_range_read <- function(url, offset, length) {
  # try arrow filesystem for s3:// and gs://
  if (grepl("^s3://", url)) {
    fs_and_path <- arrow::FileSystem$from_uri(url)
    f <- fs_and_path$fs$OpenInputFile(fs_and_path$path)
    on.exit(f$close())
    f$Seek(offset)
    return(f$Read(length))
  }

  if (grepl("^gs://", url)) {
    fs_and_path <- arrow::FileSystem$from_uri(url)
    f <- fs_and_path$fs$OpenInputFile(fs_and_path$path)
    on.exit(f$close())
    f$Seek(offset)
    return(f$Read(length))
  }

  # fallback to gdalraster VSI for http(s) and local
  if (requireNamespace("gdalraster", quietly = TRUE)) {
    vsi_url <- if (grepl("^https?://", url)) {
      paste0("/vsicurl/", url)
    } else {
      url
    }
    #con <- gdalraster::vsi_open(vsi_url, "rb")
    vsi <- methods::new(gdalraster::VSIFile, vsi_url, "rb")
    on.exit(vsi$close(), add = TRUE)
    vsi$seek(offset, "SEEK_SET")
    return(vsi$read(length))
  }

  stop("No backend available for byte-range read of: ", url, call. = FALSE)
}
