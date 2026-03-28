# chunk.R — chunk-indexed read and iteration
#
# Work directly with chunks as the unit of I/O and computation.
# Complements zaro_read() which assembles chunks into a single array.
#
# zaro_chunk()       read a single chunk by grid index
# zaro_chunk_info()  get chunk grid dimensions and metadata
# zaro_chunks()      read multiple chunks, return as list
# zaro_chunk_apply() iterate over chunks with a function


#' Get chunk grid information for an array
#'
#' Returns the chunk grid dimensions and related metadata needed for
#' chunk-indexed operations.
#'
#' @param store a store object from [zaro()]
#' @param path character. Path to the array.
#' @param meta optional pre-fetched ZaroMeta (or root group meta).
#' @param verbose logical.
#' @returns A list with components:
#'   \describe{
#'     \item{grid}{integer vector — number of chunks along each dimension}
#'     \item{chunk_shape}{integer vector — shape of a full (non-edge) chunk}
#'     \item{array_shape}{integer vector — shape of the full array}
#'     \item{n_chunks}{integer — total number of chunks}
#'     \item{dimension_names}{character vector or NULL}
#'     \item{meta}{ZaroMeta for the array}
#'   }
#'
#' @export
zaro_chunk_info <- function(store, path, meta = NULL, verbose = TRUE) {
  meta <- resolve_array_meta(store, path, meta, verbose)

  grid <- ceiling(meta@shape / meta@chunk_shape)

  list(
    grid = grid,
    chunk_shape = meta@chunk_shape,
    array_shape = meta@shape,
    n_chunks = prod(grid),
    dimension_names = meta@dimension_names,
    meta = meta
  )
}


#' Read a single chunk by grid index
#'
#' Fetches and decodes one chunk, identified by its position in the chunk
#' grid. Returns the decoded values with metadata about where the chunk
#' sits in the array.
#'
#' @param store a store object from [zaro()]
#' @param path character. Path to the array.
#' @param chunk_idx integer vector. 0-based chunk grid index
#'   (e.g. \code{c(0, 2, 3)} for the first time step, third lat chunk,
#'   fourth lon chunk).
#' @param meta optional pre-fetched ZaroMeta (or root group meta).
#' @param shaped logical. If TRUE (default), reshape the values to the
#'   chunk's actual dimensions (handling C-order and edge chunks).
#'   If FALSE, return a flat vector.
#' @param verbose logical.
#' @returns A list with components:
#'   \describe{
#'     \item{cidx}{integer vector — the chunk grid index}
#'     \item{values}{array or vector — decoded chunk data}
#'     \item{start}{integer vector — 0-based array coordinates of chunk origin}
#'     \item{shape}{integer vector — actual shape of this chunk (may be
#'       smaller than chunk_shape for edge chunks)}
#'   }
#'   Returns NULL if the chunk doesn't exist (missing/sparse).
#'
#' @export
zaro_chunk <- function(store, path, chunk_idx, meta = NULL,
                       shaped = TRUE, verbose = TRUE) {
  meta <- resolve_array_meta(store, path, meta, verbose)
  chunk_idx <- as.integer(chunk_idx)

  raw <- fetch_chunk(store, path, chunk_idx, meta)
  if (is.null(raw)) return(NULL)

  values <- decode_chunk(raw, meta)

  # where does this chunk sit in array coordinates?
  chunk_start <- chunk_idx * meta@chunk_shape
  actual_shape <- pmin(meta@chunk_shape, meta@shape - chunk_start)

  if (shaped) {
    values <- shape_chunk(values, meta@chunk_shape, actual_shape, meta)
  }

  list(
    cidx = chunk_idx,
    values = values,
    start = chunk_start,
    shape = actual_shape
  )
}


#' Read multiple chunks by index range
#'
#' Fetches and decodes a set of chunks identified by ranges along each
#' dimension of the chunk grid. Supports parallel fetching.
#'
#' @param store a store object from [zaro()]
#' @param path character. Path to the array.
#' @param ranges a list of integer vectors, one per dimension, giving the
#'   0-based chunk indices to read. Use \code{NULL} for "all chunks along
#'   this dimension". E.g. \code{list(0, NULL, 2:4)} reads chunk 0 along
#'   dim 1, all chunks along dim 2, chunks 2-4 along dim 3.
#' @param meta optional pre-fetched ZaroMeta (or root group meta).
#' @param shaped logical. If TRUE, reshape each chunk's values.
#' @param parallel logical. Use future.apply for parallel fetch+decode.
#' @param verbose logical.
#' @returns A list of chunk results (as from \code{zaro_chunk()}). NULL
#'   entries indicate missing chunks.
#'
#' @export
zaro_chunks <- function(store, path, ranges = NULL, meta = NULL,
                        shaped = TRUE, parallel = FALSE, verbose = TRUE) {
  meta <- resolve_array_meta(store, path, meta, verbose)
  info <- zaro_chunk_info(store, path, meta = meta, verbose = FALSE)

  # expand NULL ranges to full extent
  if (is.null(ranges)) {
    ranges <- lapply(info$grid, function(g) seq.int(0L, g - 1L))
  } else {
    ranges <- lapply(seq_along(ranges), function(d) {
      r <- ranges[[d]]
      if (is.null(r)) seq.int(0L, info$grid[d] - 1L) else as.integer(r)
    })
  }

  chunk_indices <- chunk_index_iter(ranges)
  n_total <- length(chunk_indices)
  vmsg("reading ", n_total, " chunk(s) for path '", path, "'",
       verbose = verbose)

  use_parallel <- isTRUE(parallel) && n_total >= 4L &&
    requireNamespace("future.apply", quietly = TRUE)

  fetch_one <- function(cidx) {
    raw <- fetch_chunk(store, path, cidx, meta)
    if (is.null(raw)) return(NULL)

    values <- decode_chunk(raw, meta)
    chunk_start <- cidx * meta@chunk_shape
    actual_shape <- pmin(meta@chunk_shape, meta@shape - chunk_start)

    if (shaped) {
      values <- shape_chunk(values, meta@chunk_shape, actual_shape, meta)
    }

    list(
      cidx = cidx,
      values = values,
      start = chunk_start,
      shape = actual_shape
    )
  }

  if (use_parallel) {
    vmsg("fetching chunks in parallel via future.apply", verbose = verbose)
    future.apply::future_lapply(chunk_indices, fetch_one)
  } else {
    lapply(chunk_indices, fetch_one)
  }
}


#' Apply a function over chunks
#'
#' Iterates over chunks and applies a function to each, accumulating
#' results. Chunks are fetched and decoded one at a time (or in parallel)
#' so memory usage is proportional to one chunk, not the full array.
#'
#' @param store a store object from [zaro()]
#' @param path character. Path to the array.
#' @param fun function. Called with a single chunk result list (as from
#'   \code{zaro_chunk()}). NULL chunks (missing) are skipped.
#' @param ranges optional range specification (as in \code{zaro_chunks()}).
#' @param meta optional pre-fetched ZaroMeta (or root group meta).
#' @param shaped logical. If TRUE, reshape each chunk's values.
#' @param parallel logical. Use future.apply for parallel execution.
#' @param verbose logical.
#' @returns A list of results from \code{fun}, one per non-NULL chunk.
#'
#' @export
#' @examples
#' \dontrun{
#' # per-chunk mean, memory-efficient
#' means <- zaro_chunk_apply(store, "temperature", function(chunk) {
#'   mean(chunk$values, na.rm = TRUE)
#' })
#'
#' # first time step only, all spatial chunks
#' means <- zaro_chunk_apply(store, "temperature", function(chunk) {
#'   c(mean = mean(chunk$values, na.rm = TRUE),
#'     n_valid = sum(is.finite(chunk$values)))
#' }, ranges = list(0, NULL, NULL))
#' }
zaro_chunk_apply <- function(store, path, fun, ranges = NULL, meta = NULL,
                             shaped = TRUE, parallel = FALSE, verbose = TRUE) {
  chunks <- zaro_chunks(store, path, ranges = ranges, meta = meta,
                        shaped = shaped, parallel = parallel,
                        verbose = verbose)

  # apply fun to non-NULL chunks
  results <- vector("list", length(chunks))
  for (i in seq_along(chunks)) {
    if (!is.null(chunks[[i]])) {
      results[[i]] <- fun(chunks[[i]])
    }
  }

  # drop NULLs
  Filter(Negate(is.null), results)
}


# -- internal helpers -------------------------------------------------------

#' Resolve meta to array-level metadata
#'
#' Handles NULL meta (fetch from store), root group meta (look up path
#' in consolidated entries), and direct array meta (pass through).
#' @noRd
resolve_array_meta <- function(store, path, meta, verbose) {
  if (path == ".") path <- ""

  if (is.null(meta)) {
    meta <- zaro_meta(store, path, consolidated = TRUE, verbose = verbose)
  }

  if (meta@node_type == "group") {
    cm <- attr(meta, "consolidated")
    if (!is.null(cm) && path %in% names(cm)) {
      meta <- cm[[path]]
    } else {
      meta <- zaro_meta(store, path, consolidated = TRUE, verbose = verbose)
    }
  }

  if (meta@node_type != "array") {
    stop("path '", path, "' is a group, not an array", call. = FALSE)
  }

  meta
}


#' Reshape decoded values for a chunk, handling edge chunks and C-order
#' @noRd
shape_chunk <- function(values, chunk_shape, actual_shape, meta) {
  # edge chunks may be full-size (padded) or truncated
  if (length(values) == prod(chunk_shape)) {
    reshape_shape <- chunk_shape
  } else {
    reshape_shape <- actual_shape
  }

  order <- meta@raw_meta[["order"]] %||% "C"
  if (order == "C") {
    dim(values) <- rev(reshape_shape)
    values <- aperm(values)
  } else {
    dim(values) <- reshape_shape
  }

  values
}
