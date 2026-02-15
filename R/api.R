# api.R — flat public API for zaro
#
# zaro()          open a store
# zaro_array()    get an array handle
# zaro_read()     read data from an array
# zaro_meta()     inspect metadata
# zaro_list()     list contents


#' Open a Zarr store
#'
#' Connects to a Zarr store at the given path or URI. Dispatches to the
#' appropriate backend based on the URI scheme:
#' \itemize{
#'   \item Local paths: Arrow LocalFileSystem
#'   \item \code{s3://}: Arrow S3FileSystem
#'   \item \code{gs://}: Arrow GcsFileSystem
#'   \item \code{http://}, \code{https://}: gdalraster VSI (\code{/vsicurl/})
#'   \item \code{reference+json://}: Kerchunk JSON reference store
#'   \item \code{reference+parquet://}: Kerchunk Parquet reference store
#' }
#'
#' @param source character. Path, URI, or reference store locator.
#' @param ... additional arguments passed to Arrow filesystem constructors
#'   (e.g. \code{anonymous = TRUE}, \code{endpoint_override}, \code{region}).
#' @returns A store object (internal class) for use with other zaro functions.
#'
#' @export
#' @examples
#' \dontrun{
#' # local store
#' store <- zaro("/data/my_dataset.zarr")
#'
#' # S3 store (anonymous)
#' store <- zaro("s3://mur-sst/zarr-v1", anonymous = TRUE)
#'
#' # Kerchunk JSON reference
#' store <- zaro("reference+json:///path/to/refs.json")
#'
#' # then explore
#' zaro_list(store)
#' zaro_meta(store, "analysed_sst")
#' data <- zaro_read(store, "analysed_sst",
#'                   start = c(0, 0, 0), count = c(1, 100, 100))
#' }
zaro <- function(source, ...) {
  # Kerchunk JSON reference store
  if (grepl("^reference\\+json://", source)) {
    path <- sub("^reference\\+json://", "", source)
    return(parse_kerchunk_json(path))
  }

  # Kerchunk Parquet reference store
  if (grepl("^reference\\+parquet://", source)) {
    path <- sub("^reference\\+parquet://", "", source)
    return(parse_kerchunk_parquet(path))
  }

  # S3
  if (grepl("^s3://", source)) {
    fs <- arrow::S3FileSystem$create(...)
    root <- sub("^s3://", "", source)
    return(ArrowStore(root = root, fs = fs))
  }

  # GCS
  if (grepl("^gs://", source)) {
    fs <- arrow::GcsFileSystem$create(...)
    root <- sub("^gs://", "", source)
    return(ArrowStore(root = root, fs = fs))
  }

  # HTTP/HTTPS — use gdalraster VSI
  if (grepl("^https?://", source)) {
    vsi_root <- paste0("/vsicurl/", source)
    return(VSIStore(root = vsi_root))
  }

  # local filesystem
  fs <- arrow::LocalFileSystem$create()
  ArrowStore(root = normalizePath(source, mustWork = FALSE), fs = fs)
}


#' List contents of a Zarr store
#'
#' @param store a store object from [zaro()]
#' @param path character. Path prefix within the store (default root).
#' @returns character vector of keys (relative paths).
#'
#' @export
zaro_list <- function(store, path = "") {
  store_list(store, prefix = path)
}


#' Get metadata for a Zarr node
#'
#' Reads and parses the \code{zarr.json} metadata document for an array
#' or group at the given path within the store.
#'
#' @param store a store object from [zaro()]
#' @param path character. Path to the array or group within the store.
#'   Use \code{""} or \code{"/"} for the root group.
#' @param consolidated logical. If \code{TRUE}, attempt to read consolidated
#'   metadata from the root \code{zarr.json} first.
#' @returns A ZaroMeta object.
#'
#' @export
zaro_meta <- function(store, path = "", consolidated = TRUE) {
  # try consolidated metadata first
  if (consolidated && (path == "" || path == "/")) {
    raw <- store_get(store, "zarr.json")
    if (!is.null(raw)) {
      cm <- parse_consolidated(raw)
      if (!is.null(cm)) {
        # return the root meta, with consolidated entries as an attribute
        root_meta <- parse_zarr_json(raw)
        attr(root_meta, "consolidated") <- cm
        return(root_meta)
      }
      return(parse_zarr_json(raw))
    }
  }

  # direct read of the node's zarr.json
  key <- if (nzchar(path) && path != "/") {
    paste0(path, "/zarr.json")
  } else {
    "zarr.json"
  }
  raw <- store_get(store, key)
  if (is.null(raw)) {
    stop("no zarr.json found at path '", path, "'", call. = FALSE)
  }
  parse_zarr_json(raw)
}


#' Read data from a Zarr array
#'
#' Reads a hyperslab from a Zarr array, fetching and decoding the necessary
#' chunks and assembling them into an R array.
#'
#' @param store a store object from [zaro()]
#' @param path character. Path to the array within the store.
#' @param start integer vector. 0-based start indices for each dimension.
#'   Defaults to the origin.
#' @param count integer vector. Number of elements to read along each
#'   dimension. Defaults to the full extent.
#' @param meta optional pre-fetched ZaroMeta object. If NULL (default),
#'   metadata is read from the store.
#' @returns An R array with dimensions matching \code{count}.
#'
#' @export
zaro_read <- function(store, path, start = NULL, count = NULL, meta = NULL) {
  if (is.null(meta)) {
    meta <- zaro_meta(store, path, consolidated = FALSE)
  }
  if (meta@node_type != "array") {
    stop("path '", path, "' is a group, not an array", call. = FALSE)
  }

  ndim <- length(meta@shape)

  # default to full extent
  if (is.null(start)) start <- rep(0L, ndim)
  if (is.null(count)) count <- meta@shape - start

  start <- as.integer(start)
  count <- as.integer(count)

  if (length(start) != ndim || length(count) != ndim) {
    stop("start and count must have length ", ndim, call. = FALSE)
  }

  # compute which chunks we need
  chunk_ranges <- chunk_coverage(start, count, meta@chunk_shape)

  # allocate output array filled with fill_value
  out <- array(meta@fill_value, dim = count)

  # fetch and decode each chunk, copy the relevant portion into out
  slab_end <- start + count - 1L

  for (cidx in chunk_index_iter(chunk_ranges)) {
    chunk_raw <- fetch_chunk(store, path, cidx, meta)

    if (is.null(chunk_raw)) {
      next  # missing chunk → fill_value (already set)
    }

    values <- decode_chunk(chunk_raw, meta)

    # this chunk covers [chunk_start, chunk_end] in array coordinates
    chunk_start <- cidx * meta@chunk_shape
    chunk_end   <- pmin(chunk_start + meta@chunk_shape, meta@shape) - 1L

    # region of overlap in array coordinates
    ov_start <- pmax(chunk_start, start)
    ov_end   <- pmin(chunk_end, slab_end)

    # positions within the chunk (0-based) and output (1-based)
    in_chunk_start <- ov_start - chunk_start
    in_chunk_count <- ov_end - ov_start + 1L
    in_out_start   <- ov_start - start + 1L

    # reshape decoded values to actual chunk extent (edge chunks may be short)
    actual_chunk_shape <- pmin(meta@chunk_shape, meta@shape - chunk_start)
    dim(values) <- actual_chunk_shape

    # build index lists for source and destination
    src_idx <- lapply(seq_len(ndim), function(d) {
      seq.int(in_chunk_start[d] + 1L, in_chunk_start[d] + in_chunk_count[d])
    })
    dst_idx <- lapply(seq_len(ndim), function(d) {
      seq.int(in_out_start[d], in_out_start[d] + in_chunk_count[d] - 1L)
    })

    # extract slice from chunk
    chunk_slice <- do.call(`[`, c(list(values), src_idx, list(drop = FALSE)))

    # insert into output — use abind-style positional assignment
    out <- do.call(`[<-`, c(list(out), dst_idx, list(value = chunk_slice)))
  }

  # set dim names if available
  if (!is.null(meta@dimension_names)) {
    names(dim(out)) <- meta@dimension_names
  }

  out
}


# -- internal helpers --------------------------------------------------------

#' Fetch a single chunk's raw bytes from the store
#' @noRd
fetch_chunk <- function(store, path, chunk_idx, meta) {
  key <- paste0(path, "/", chunk_key(meta, chunk_idx))
  store_get(store, key)
}


#' Compute which chunks a hyperslab intersects
#'
#' @returns list of length ndim, each element an integer vector of
#'   chunk indices (0-based) along that dimension.
#' @noRd
chunk_coverage <- function(start, count, chunk_shape) {
  end <- start + count - 1L
  lapply(seq_along(start), function(d) {
    first_chunk <- start[d] %/% chunk_shape[d]
    last_chunk  <- end[d] %/% chunk_shape[d]
    seq.int(first_chunk, last_chunk)
  })
}


#' Iterate over all chunk indices in the coverage
#'
#' @param ranges list of integer vectors from chunk_coverage()
#' @returns list of integer vectors, each a chunk multi-index
#' @noRd
chunk_index_iter <- function(ranges) {
  grid <- expand.grid(ranges, KEEP.OUT.ATTRS = FALSE)
  lapply(seq_len(nrow(grid)), function(i) as.integer(grid[i, ]))
}

