# virtualizarr.R - VirtualiZarr Parquet reference store
#
# VirtualiZarr emits a directory that looks like a V2 Zarr store:
#
#   dataset.parq/
#     .zmetadata              # standard V2 consolidated metadata (JSON)
#     .zgroup
#     .zattrs
#     temp/
#       refs.0.parq           # chunk manifest shards (Parquet)
#       refs.1.parq
#       ...
#     Time/
#       refs.0.parq
#     xt_ocean/
#       refs.0.parq
#
# Metadata files (.zmetadata, .zgroup, .zattrs, .zarray) are fetched
# directly as text/JSON. Chunk reads are resolved through the Parquet
# manifest files which contain (path, offset, length) references into
# the original source files (NetCDF, HDF5, etc).
#
# This is GDAL-free: metadata via curl, manifests via arrow::read_parquet(),
# chunk bytes via byte_range_read().


#' Open a VirtualiZarr Parquet reference store
#'
#' @param base_url character. Base URL or local path to the .parq directory.
#' @param verbose logical. Emit diagnostic messages. Default
#'   `getOption("zaro.verbose", TRUE)`.
#' @returns A VirtualiZarrStore object.
#' @noRd
open_virtualizarr <- function(base_url, verbose = zaro_verbose()) {
  # strip trailing slash
  base_url <- sub("/$", "", base_url)

  vmsg("opening VirtualiZarr Parquet reference store: ", base_url,
       verbose = verbose)

  cache <- new.env(parent = emptyenv())
  cache$meta <- list()          # key -> raw bytes
  cache$chunk_grids <- list()   # var_name -> integer vector of grid dims
  cache$shard_sizes <- list()   # var_name -> integer (rows per shard)
  cache$shards <- list()        # "var_name:N" -> data.frame

  VirtualiZarrStore(
    root = base_url,
    cache = cache
  )
}


# -- S7 class ---------------------------------------------------------------

VirtualiZarrStore <- new_class("VirtualiZarrStore", parent = ZaroStore,
                               properties = list(
                                 cache = class_any   # environment: shared by reference across copies
                               )
)


# -- store methods -----------------------------------------------------------

method(store_get, VirtualiZarrStore) <- function(store, key) {
  # metadata files - fetch directly and cache
  if (is_metadata_key(key)) {
    cached <- store@cache$meta[[key]]
    if (!is.null(cached)) return(cached)

    raw <- vz_fetch_file(store@root, key)
    if (!is.null(raw)) {
      store@cache$meta[[key]] <- raw
    }
    return(raw)
  }

  # chunk reads - resolve through the manifest shards, then fetch
  fetch_ref(store, store_refs(store, key))
}


# Chunk keys resolve through the Parquet manifest shards (fetched once per
# shard and cached): a row is a byte range in a source file, or inline
# bytes (coordinates), or empty (absent chunk).
method(store_refs, VirtualiZarrStore) <- function(store, keys) {
  out <- refs_frame(keys, transport = "absent")
  meta_key <- is_metadata_key(keys)
  out$transport[meta_key] <- "store"
  out$url[meta_key] <- keys[meta_key]

  parts <- lapply(keys, function(k) if (is_metadata_key(k)) NULL else parse_chunk_key(k))
  vars <- vapply(parts, function(p) if (is.null(p)) NA_character_ else p$variable,
                 character(1))
  for (var_name in unique(stats::na.omit(vars))) {
    w <- which(vars == var_name)
    grid <- store@cache$chunk_grids[[var_name]]
    if (is.null(grid)) {
      grid <- vz_compute_chunk_grid(store, var_name)
      if (is.null(grid)) next
      store@cache$chunk_grids[[var_name]] <- grid
    }
    shard_size <- store@cache$shard_sizes[[var_name]]
    if (is.null(shard_size)) {
      shard_0 <- vz_fetch_shard(store, var_name, 0L)
      if (is.null(shard_0)) next
      shard_size <- nrow(shard_0)
      store@cache$shard_sizes[[var_name]] <- shard_size
      store@cache$shards[[paste0(var_name, ":0")]] <- shard_0
    }
    # C-order linear index of each chunk in the grid
    idx <- do.call(rbind, lapply(parts[w], function(p) {
      as.numeric(strsplit(p$chunk_key, "[./]")[[1]])
    }))
    stride <- rev(cumprod(rev(c(grid[-1L], 1))))
    lin <- as.numeric(idx %*% stride)
    shard_of <- lin %/% shard_size
    row_of <- lin %% shard_size + 1
    for (sh in unique(shard_of)) {
      ck <- paste0(var_name, ":", sh)
      shard <- store@cache$shards[[ck]]
      if (is.null(shard)) {
        shard <- vz_fetch_shard(store, var_name, as.integer(sh))
        if (is.null(shard)) next
        store@cache$shards[[ck]] <- shard
      }
      for (i in which(shard_of == sh)) {
        r <- row_of[i]
        k <- w[i]
        if (r > nrow(shard)) next
        inline <- if ("raw" %in% names(shard)) shard$raw[[r]] else NULL
        if (!is.null(inline) && length(inline) > 0L) {
          out$inline[[k]] <- as.raw(inline)
          out$transport[k] <- "inline"
          next
        }
        path <- shard$path[r]
        if (is.na(path) || !nzchar(path)) next
        out$url[k] <- path
        out$offset[k] <- shard$offset[r]
        out$length[k] <- shard$size[r]
        out$transport[k] <- url_transport(path)
      }
    }
  }
  out
}


method(store_list, VirtualiZarrStore) <- function(store, prefix = "") {
  # list metadata keys from .zmetadata if available
  raw <- store_get(store, ".zmetadata")
  if (!is.null(raw)) {
    doc <- jsonlite::fromJSON(sanitize_json(rawToChar(raw)),
                              simplifyVector = FALSE)
    entries <- names(doc[["metadata"]])
    if (nzchar(prefix)) {
      entries <- entries[startsWith(entries, prefix)]
    }
    return(entries)
  }
  character(0)
}


method(store_exists, VirtualiZarrStore) <- function(store, key) {
  if (is_metadata_key(key)) {
    return(!is.null(store_get(store, key)))
  }
  # for chunk keys, try the full resolution
  !is.null(store_get(store, key))
}


# -- internal helpers -------------------------------------------------------

#' Check if a key is a metadata file (not a chunk)
#' @noRd
is_metadata_key <- function(key) {
  grepl("(\\.zmetadata|\\.zgroup|\\.zattrs|\\.zarray|zarr\\.json)$", key)
}


#' Parse a chunk key into variable name and chunk index string
#'
#' e.g. "temp/0.0.0.0" -> list(variable = "temp", chunk_key = "0.0.0.0")
#' e.g. "Time/0" -> list(variable = "Time", chunk_key = "0")
#' @noRd
parse_chunk_key <- function(key) {
  slash_pos <- regexpr("/", key, fixed = TRUE)
  if (slash_pos < 1) return(NULL)

  var_name <- substr(key, 1, slash_pos - 1)
  remainder <- substr(key, slash_pos + 1, nchar(key))

  # remainder should look like a chunk index (digits and dots/slashes)
  if (!grepl("^[0-9./]+$", remainder)) return(NULL)

  list(variable = var_name, chunk_key = remainder)
}


#' Compute chunk grid dimensions for a variable from metadata
#'
#' @returns integer vector of chunk grid dimensions, or NULL
#' @noRd
vz_compute_chunk_grid <- function(store, var_name) {
  zmeta_raw <- store_get(store, ".zmetadata")
  if (is.null(zmeta_raw)) return(NULL)

  doc <- jsonlite::fromJSON(sanitize_json(rawToChar(zmeta_raw)),
                            simplifyVector = FALSE)
  entries <- doc[["metadata"]]

  zarray_key <- paste0(var_name, "/.zarray")
  zarray <- entries[[zarray_key]]
  if (is.null(zarray)) return(NULL)

  shape <- as.integer(unlist(zarray$shape))
  chunks <- as.integer(unlist(zarray$chunks))

  # chunk grid = ceiling(shape / chunks)
  ceiling(shape / chunks)
}


#' Fetch a single manifest shard for a variable
#'
#' @param store VirtualiZarrStore
#' @param var_name character. Variable name (e.g. "temp").
#' @param shard_idx integer. 0-based shard index.
#' @returns data.frame with columns: path, offset, size, raw (optional).
#'   NULL if the shard doesn't exist.
#' @noRd
vz_fetch_shard <- function(store, var_name, shard_idx) {
  shard_path <- paste0(var_name, "/refs.", shard_idx, ".parq")
  full_path <- paste0(store@root, "/", shard_path)
  pq_file <- vz_fetch_parquet_raw(full_path)
  if (is.null(pq_file)) return(NULL)
  on.exit(if (grepl("^(/tmp|.*/Rtmp)", pq_file)) unlink(pq_file))
  tryCatch({
    tbl <- arrow::read_parquet(pq_file)
    shard <- data.frame(
      path = as.character(tbl$path),
      offset = as.numeric(tbl$offset),
      size = as.integer(tbl$size),
      stringsAsFactors = FALSE
    )
    if ("raw" %in% names(tbl)) {
      tryCatch({
        shard$raw <- as.list(tbl[["raw"]])
      }, error = function(e) NULL)
    }
    shard
  }, error = function(e) NULL)
}

#' Fetch a file from the VirtualiZarr store via HTTP or local filesystem
#'
#' Uses curl for HTTP, readBin for local.
#' @noRd
vz_fetch_file <- function(base_url, relative_path) {
  full_path <- paste0(base_url, "/", relative_path)

  if (grepl("^https?://", full_path)) {
    return(vz_http_get(full_path))
  }

  # local filesystem
  if (file.exists(full_path)) {
    return(readBin(full_path, "raw", file.size(full_path)))
  }

  NULL
}


#' HTTP GET returning raw bytes, or NULL on failure
#'
#' Uses curl if available (no GDAL dependency), falls back to base R.
#' @noRd
vz_http_get <- function(url) {
  if (requireNamespace("curl", quietly = TRUE)) {
    tryCatch({
      resp <- curl::curl_fetch_memory(url)
      if (resp$status_code == 200L) resp$content else NULL
    }, error = function(e) NULL)
  } else {
    # base R fallback
    tryCatch({
      tf <- tempfile()
      on.exit(unlink(tf))
      utils::download.file(url, tf, quiet = TRUE, mode = "wb")
      readBin(tf, "raw", file.size(tf))
    }, error = function(e) NULL)
  }
}


#' Fetch a Parquet file to a local path for arrow::read_parquet()
#'
#' Downloads HTTP resources to a tempfile. Returns file path or NULL.
#' @noRd
vz_fetch_parquet_raw <- function(full_path) {
  if (grepl("^https?://", full_path)) {
    raw <- vz_http_get(full_path)
    if (is.null(raw)) return(NULL)
    tf <- tempfile(fileext = ".parquet")
    writeBin(raw, tf)
    return(tf)
  }

  # local filesystem
  if (file.exists(full_path)) return(full_path)

  NULL
}
