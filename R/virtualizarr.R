# virtualizarr.R — VirtualiZarr Parquet reference store
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
#' @param verbose logical. Emit diagnostic messages.
#' @returns A VirtualiZarrStore object.
#' @noRd
open_virtualizarr <- function(base_url, verbose = TRUE) {
  # strip trailing slash
  base_url <- sub("/$", "", base_url)

  vmsg("opening VirtualiZarr Parquet reference store: ", base_url,
       verbose = verbose)

  cache <- new.env(parent = emptyenv())
  cache$manifests <- list()     # var_name -> data.frame of chunk refs
  cache$meta <- list()          # key -> raw bytes
  cache$chunk_grids <- list()   # var_name -> integer vector of grid dims

  VirtualiZarrStore(
    root = base_url,
    cache = cache
  )
}


# -- S7 class ---------------------------------------------------------------

VirtualiZarrStore <- new_class("VirtualiZarrStore", parent = ZaroStore,
  properties = list(
    cache = class_any   # environment: manifests + meta (shared by reference)
  )
)


method(store_get, VirtualiZarrStore) <- function(store, key) {
  # metadata files — fetch directly and cache
  if (is_metadata_key(key)) {
    cached <- store@cache$meta[[key]]
    if (!is.null(cached)) return(cached)

    raw <- vz_fetch_file(store@root, key)
    if (!is.null(raw)) {
      store@cache$meta[[key]] <- raw
    }
    return(raw)
  }

  # chunk reads — resolve through manifest
  parts <- parse_chunk_key(key)
  if (is.null(parts)) return(NULL)

  var_name <- parts$variable
  chunk_key <- parts$chunk_key

  # ensure manifest is loaded for this variable
  if (is.null(store@cache$manifests[[var_name]])) {
    manifest <- vz_load_manifest(store@root, var_name)
    if (is.null(manifest)) return(NULL)
    store@cache$manifests[[var_name]] <- manifest
  }

  manifest <- store@cache$manifests[[var_name]]

  # compute chunk grid dimensions from metadata (needed for C-order lookup)
  chunk_grid <- store@cache$chunk_grids[[var_name]]
  if (is.null(chunk_grid)) {
    chunk_grid <- vz_compute_chunk_grid(store, var_name)
    store@cache$chunk_grids[[var_name]] <- chunk_grid
  }

  # look up the chunk in the manifest
  ref <- vz_resolve_chunk(manifest, chunk_key, chunk_grid)
  if (is.null(ref)) return(NULL)

  # inline data (coordinate variables stored directly in Parquet)
  if (!is.null(ref$inline)) {
    return(ref$inline)
  }

  # byte-range read from the source file
  byte_range_read(ref$path, ref$offset, ref$size)
}


method(store_list, VirtualiZarrStore) <- function(store, prefix = "") {
  # list metadata keys from .zmetadata if available
  raw <- store_get(store, ".zmetadata")
  if (!is.null(raw)) {
    doc <- jsonlite::fromJSON(sanitize_json(rawToChar(raw)), simplifyVector = FALSE)
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
  # for chunk keys, we could check the manifest but that's expensive;
  # just try the get
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
  # split on first /
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
  # read .zmetadata to get shape and chunk info
  zmeta_raw <- store_get(store, ".zmetadata")
  if (is.null(zmeta_raw)) return(NULL)

  doc <- jsonlite::fromJSON(sanitize_json(rawToChar(zmeta_raw)), simplifyVector = FALSE)
  entries <- doc[["metadata"]]

  zarray_key <- paste0(var_name, "/.zarray")
  zarray <- entries[[zarray_key]]
  if (is.null(zarray)) return(NULL)

  shape <- as.integer(unlist(zarray$shape))
  chunks <- as.integer(unlist(zarray$chunks))

  # chunk grid = ceiling(shape / chunks)
  ceiling(shape / chunks)
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
      download.file(url, tf, quiet = TRUE, mode = "wb")
      readBin(tf, "raw", file.size(tf))
    }, error = function(e) NULL)
  }
}


#' Fetch a Parquet file to a local path for arrow::read_parquet()
#'
#' Downloads HTTP resources to a tempfile. Returns path or NULL.
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


#' Load all manifest shards for a variable
#'
#' VirtualiZarr stores chunk references in refs.N.parq files within
#' each variable's directory. Row ordering corresponds to C-order
#' linearization of the chunk grid — no explicit chunk key column.
#'
#' @returns data.frame with columns: path, offset, size, raw (optional),
#'   plus row_start attribute indicating global row offset per shard.
#' @noRd
vz_load_manifest <- function(base_url, var_name) {
  all_shards <- list()
  tempfiles <- character(0)

  i <- 0
  max_shards <- 1000  # safety limit
  while (i < max_shards) {
    shard_path <- paste0(var_name, "/refs.", i, ".parq")
    full_path <- paste0(base_url, "/", shard_path)

    pq_file <- vz_fetch_parquet_raw(full_path)
    if (is.null(pq_file)) break

    # track tempfiles for cleanup
    if (grepl("^(/tmp|.*/Rtmp)", pq_file)) {
      tempfiles <- c(tempfiles, pq_file)
    }

    tryCatch({
      tbl <- arrow::read_parquet(pq_file)
      print(pryr::object_size(tbl))
      shard <- data.frame(
        path = as.character(tbl$path),
        offset = as.numeric(tbl$offset),
        size = as.integer(tbl$size),
        stringsAsFactors = FALSE
      )
      # inline data column — convert Arrow binary to list of raw vectors
      if ("raw" %in% names(tbl)) {
        shard$raw <- lapply(seq_len(nrow(tbl)), function(j) {
          v <- tbl$raw[j]
          if (is.null(v) || length(v) == 0) NULL else as.raw(v)
        })
      }
      all_shards[[length(all_shards) + 1]] <- shard
    }, error = function(e) {
      # corrupted shard, skip
    })

    i <- i + 1
  }

  # clean up tempfiles
  if (length(tempfiles) > 0) unlink(tempfiles)

  if (length(all_shards) == 0) return(NULL)

  # rbind all shards — row order across shards IS the chunk order
  combined <- do.call(rbind, all_shards)

  # normalise column names
  nms <- tolower(names(combined))
  names(combined) <- nms

  # find columns
  path_col <- intersect(c("path", "url", "uri", "file"), nms)[1]
  offset_col <- intersect(c("offset", "start", "byte_offset"), nms)[1]
  size_col <- intersect(c("size", "length", "byte_length"), nms)[1]
  raw_col <- intersect(c("raw", "data", "inline"), nms)[1]

  if (is.na(path_col) || is.na(offset_col) || is.na(size_col)) {
    return(NULL)
  }

  result <- data.frame(
    path = as.character(combined[[path_col]]),
    offset = as.numeric(combined[[offset_col]]),
    size = as.numeric(combined[[size_col]]),
    stringsAsFactors = FALSE
  )

  # attach inline data column if present (for coordinate variables)
  if (!is.na(raw_col)) {
    result$raw <- combined[[raw_col]]
  }

  result
}


#' Resolve a chunk key to row index and fetch the reference
#'
#' The chunk key (e.g. "0.0.0.0") is converted to a C-order linear index
#' which is the row number in the concatenated manifest.
#'
#' @param manifest data.frame from vz_load_manifest()
#' @param chunk_key character, dot-separated chunk indices (V2 convention)
#' @param chunk_grid integer vector, number of chunks along each dimension
#' @returns list(path, offset, size) or NULL. If inline data exists, returns
#'   the raw bytes directly.
#' @noRd
vz_resolve_chunk <- function(manifest, chunk_key, chunk_grid = NULL) {
  # convert dot-separated key to linear index
  indices <- as.integer(strsplit(chunk_key, ".", fixed = TRUE)[[1]])
print(indices)
  if (!is.null(chunk_grid)) {
    # C-order linearization
    linear_idx <- 0L
    stride <- 1L
    for (d in rev(seq_along(indices))) {
      linear_idx <- linear_idx + indices[d] * stride
      stride <- stride * chunk_grid[d]
    }
    row_idx <- linear_idx + 1L  # 1-based
  } else {
    # if no grid info, try treating the key as a simple scalar index
    if (length(indices) == 1L) {
      row_idx <- indices[1] + 1L
    } else {
      # can't resolve without chunk grid
      return(NULL)
    }
  }

  if (row_idx < 1 || row_idx > nrow(manifest)) return(NULL)

  # check for inline data first
  if ("raw" %in% names(manifest)) {
    raw_val <- manifest$raw[[row_idx]]
    if (!is.null(raw_val) && length(raw_val) > 0) {
      return(list(inline = raw_val))
    }
  }

  path <- manifest$path[row_idx]
  if (is.na(path) || path == "") return(NULL)


  list(
    path = path,
    offset = manifest$offset[row_idx],
    size = manifest$size[row_idx]
  )
}
