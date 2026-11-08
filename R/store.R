# store.R - key-value store backends
#
# All stores expose the same interface:
#   store_get(store, key)      -> raw vector or NULL
#   store_list(store, prefix)  -> character vector of keys
#   store_exists(store, key)   -> logical
#   store_refs(store, keys)    -> refs table: where each key's bytes are
#                                 (url, offset, length, inline, transport),
#                                 without fetching them (see refs.R)
#
# Backends:
#   ArrowStore     - arrow::FileSystem (local, S3, GCS)
#   HttpStore      - plain HTTP(S) via curl
#   VSIStore       - gdalraster VSI (Azure, any GDAL vfs; S3 region fallback)
#   ReferenceStore - Kerchunk JSON or Parquet virtual references

# -- abstract store ---------------------------------------------------------

ZaroStore <- new_class("ZaroStore", properties = list(
  root = class_character
))

store_get <- new_generic("store_get", "store")
store_list <- new_generic("store_list", "store")
store_exists <- new_generic("store_exists", "store")
store_refs <- new_generic("store_refs", "store")
# store_range(store, key, offset, length): `length` bytes of a key from
# `offset`; a negative offset means the last `length` bytes. NULL if absent.
store_range <- new_generic("store_range", "store")

method(store_range, ZaroStore) <- function(store, key, offset, length) {
  raw <- store_get(store, key)
  if (is.null(raw)) return(NULL)
  if (offset < 0) offset <- base::length(raw) - length
  raw[seq.int(offset + 1, length.out = length)]
}

# Any store: fetch each key through store_get() ("store" transport)
method(store_refs, ZaroStore) <- function(store, keys) {
  refs_frame(keys, path = keys, transport = "store")
}

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

method(store_list, ArrowStore) <- function(store, prefix = "", recursive = FALSE) {
  path <- paste0(store@root, prefix, collapse = "/")
  sel <- arrow::FileSelector$create(path, recursive = recursive)
  infos <- store@fs$GetFileInfo(sel)
  paths <- vapply(infos, function(i) i$path, character(1))
  # return paths relative to root
  sub(paste0("^", store@root, "/?"), "", paths)
}

method(store_range, ArrowStore) <- function(store, key, offset, length) {
  tryCatch({
    f <- store@fs$OpenInputFile(file.path(store@root, key))
    on.exit(f$close())
    if (offset < 0) offset <- f$GetSize() - length
    as.raw(f$ReadAt(offset, length))
  }, error = function(e) NULL)
}

# Native chunks are whole objects: no request needed to plan them. S3 and
# GCS are read through the store's own Arrow filesystem (credentials,
# region), local files directly.
method(store_refs, ArrowStore) <- function(store, keys) {
  scheme <- switch(class(store@fs)[1], S3FileSystem = "s3://",
                   GcsFileSystem = "gs://", "")
  refs_frame(keys, path = paste0(scheme, store@root, "/", keys),
             offset = 0, transport = if (nzchar(scheme)) "store" else "local")
}

method(store_exists, ArrowStore) <- function(store, key) {
  path <- file.path(store@root, key)
  tryCatch({
    info <- store@fs$GetFileInfo(path)
    info$type != 0L  # 0 = NotFound
  }, error = function(e) FALSE)
}


# -- HTTP store (curl) ------------------------------------------------------

# Plain HTTP(S) Zarr, GDAL-free. Metadata keys are cached; chunks are
# planned as whole-object "http" refs so zaro_fetch() batches them.
HttpStore <- new_class("HttpStore", parent = ZaroStore, properties = list(
  cache = class_any   # environment: metadata key -> raw
))

method(store_get, HttpStore) <- function(store, key) {
  meta_key <- is_metadata_key(key)
  if (meta_key && !is.null(hit <- store@cache[[key]])) return(hit)
  raw <- http_get(paste0(store@root, "/", key))
  if (meta_key && !is.null(raw)) assign(key, raw, envir = store@cache)
  raw
}

method(store_list, HttpStore) <- function(store, prefix = "") {
  stop("an HTTP store can't be listed; it needs consolidated metadata, ",
       "or name the arrays (zaro_dataset(arrays = ))", call. = FALSE)
}

method(store_exists, HttpStore) <- function(store, key) {
  h <- curl::new_handle(nobody = TRUE)
  r <- tryCatch(curl::curl_fetch_memory(paste0(store@root, "/", key), handle = h),
                error = function(e) NULL)
  !is.null(r) && r$status_code == 200L
}

method(store_refs, HttpStore) <- function(store, keys) {
  refs_frame(keys, path = paste0(store@root, "/", keys), offset = 0,
             transport = "http")
}

#' GET a URL: raw on 200, NULL when it does not exist (404/403/410),
#' an error otherwise (so network trouble is not mistaken for "absent")
#' @noRd
http_get <- function(url) {
  r <- tryCatch(curl::curl_fetch_memory(url), error = function(e) {
    stop("HTTP request failed for ", url, ": ", conditionMessage(e),
         call. = FALSE)
  })
  if (r$status_code == 200L) return(r$content)
  if (r$status_code %in% c(403L, 404L, 410L)) return(NULL)
  stop("HTTP ", r$status_code, " for ", url, call. = FALSE)
}


# -- gdalraster VSI store ---------------------------------------------------

VSIStore <- new_class("VSIStore", parent = ZaroStore)

method(store_get, VSIStore) <- function(store, key) {
  if (!requireNamespace("gdalraster", quietly = TRUE)) {
    stop("gdalraster required for VSI store access", call. = FALSE)
  }

  path <- paste0(store@root, "/", key)
  tryCatch({
    con <- new(gdalraster::VSIFile, path, "rb")

    on.exit(con$close())
  #  sz <- gdalraster::vsi_stat(path, "size")
    con$ingest(-1)
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

# A /vsicurl/ root is plain HTTP (curl can fetch it); other VSI roots stay
# with gdalraster.
method(store_refs, VSIStore) <- function(store, keys) {
  url <- paste0(store@root, "/", keys)
  http <- startsWith(store@root, "/vsicurl/")
  if (http) url <- sub("^/vsicurl/", "", url)
  refs_frame(keys, path = url, offset = 0,
             transport = if (http) "http" else "vsi")
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
  fetch_ref(store, store_refs(store, key))
}

# Kerchunk refs: an inline string ("base64:" prefixed for binary), or
# [url] (whole object) or [url, offset, length].
method(store_refs, ReferenceStore) <- function(store, keys) {
  out <- refs_frame(keys, transport = "absent")
  for (i in seq_along(keys)) {
    ref <- store@refs[[keys[i]]]
    if (is.null(ref)) next
    if (is.character(ref) && length(ref) == 1L) {
      out$data[[i]] <- if (startsWith(ref, "base64:")) {
        jsonlite::base64_dec(substring(ref, 8L))
      } else {
        charToRaw(ref)
      }
      out$transport[i] <- "inline"
      next
    }
    ref <- as.list(ref)
    out$path[i] <- expand_template(as.character(ref[[1]]), store@templates)
    out$offset[i] <- if (length(ref) >= 3L) as.numeric(ref[[2]]) else 0
    out$size[i] <- if (length(ref) >= 3L) as.numeric(ref[[3]]) else NA_real_
    out$transport[i] <- url_transport(out$path[i])
  }
  out
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
  # a negative offset reads the last `length` bytes (a shard index at the end)
  # try arrow filesystem for s3:// and gs://
  if (grepl("^(s3|gs)://", url)) {
    fs_and_path <- arrow::FileSystem$from_uri(url)
    f <- fs_and_path$fs$OpenInputFile(fs_and_path$path)
    on.exit(f$close())
    if (offset < 0) offset <- f$GetSize() - length
    return(as.raw(f$ReadAt(offset, length)))   # an Arrow Buffer otherwise
  }

  # local file: base R, no GDAL needed
  if (!grepl("^(https?://|/vsi)", url)) {
    if (!file.exists(url)) return(NULL)
    if (offset < 0) offset <- file.size(url) - length
    con <- file(url, "rb")
    on.exit(close(con))
    seek(con, offset)
    return(readBin(con, "raw", length))
  }

  # fallback to gdalraster VSI for http(s) and local
  if (requireNamespace("gdalraster", quietly = TRUE)) {
    vsi_url <- if (grepl("^https?://", url)) {
      paste0("/vsicurl/", url)
    } else {
      url
    }
    #con <- gdalraster::vsi_open(vsi_url, "rb")
    if (offset < 0) offset <- gdalraster::vsi_stat(vsi_url, "size") - length
    vsi <- methods::new(gdalraster::VSIFile, vsi_url, "rb")
    on.exit(vsi$close(), add = TRUE)
    vsi$seek(offset, "SEEK_SET")

    return(vsi$read(length))
  }

  if (grepl("^https?://", url) && requireNamespace("curl", quietly = TRUE)) {
    resp <- curl::curl_fetch_memory(url,
                                    handle = curl::new_handle(range = paste0(offset, "-", offset + length - 1L)))
    if (resp$status_code == 206L) return(resp$content)
  }
  stop("No backend available for byte-range read of: ", url, call. = FALSE)
}
