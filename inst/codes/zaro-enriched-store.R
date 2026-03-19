# zaro_enriched_store.R
#
# A zaro store backend for the enriched parquet refs format.
# This extends zaro's existing ReferenceStore to work with the
# richer metadata and spatial indexing in the enriched format.
#
# Key difference from existing ReferenceStore:
#   - Metadata comes from parquet file-level KV pairs, not sidecar JSON
#   - Spatial/temporal subsetting happens at the Arrow level before byte reads
#   - The zarr .zarray/.zattrs are reconstructed from parquet metadata
#   - No zarr directory tree needed — the parquet IS the store
#
# This is a design sketch for integration into zaro's store.R

library(S7)
library(arrow)
library(zaro)

# --- Store class -------------------------------------------------------------

EnrichedParquetStore <- new_class("EnrichedParquetStore",
                                  properties = list(
                                    path     = class_character,   # path to parquet file
                                    metadata = class_list,        # parsed file-level metadata
                                    refs     = class_any,         # lazy arrow dataset handle
                                    verbose  = class_logical
                                  ),
                                  constructor = function(path, verbose = TRUE) {
                                    # Open as Arrow dataset for lazy evaluation / predicate pushdown
                                    ds <- arrow::open_dataset(path)

                                    # Extract file-level metadata from schema
                                    md <- ds$schema$metadata

                                    # Parse key enriched-refs metadata
                                    parsed_md <- list(
                                      variable      = md[["variable"]] %||% "data",
                                      geotransform  = as.numeric(strsplit(md[["geotransform"]], ",")[[1]]),
                                      crs_wkt       = md[["crs_wkt"]] %||% "",
                                      zarray        = jsonlite::fromJSON(md[["zarray"]], simplifyVector = FALSE),
                                      zattrs        = jsonlite::fromJSON(md[["zattrs"]], simplifyVector = FALSE),
                                      image_width   = as.integer(md[["image_width"]]),
                                      image_height  = as.integer(md[["image_height"]]),
                                      tile_width    = as.integer(md[["tile_width"]]),
                                      tile_height   = as.integer(md[["tile_height"]]),
                                      dtype         = md[["dtype"]],
                                      compression   = md[["compression"]],
                                      predictor     = as.integer(md[["predictor"]] %||% "0"),
                                      format_version = md[["format_version"]] %||% "unknown"
                                    )

                                    # Parse time dimension if present
                                    if (!is.null(md[["dim_time"]])) {
                                      parsed_md$dim_time <- as.POSIXct(strsplit(md[["dim_time"]], ",")[[1]])
                                    }

                                    if (verbose) {
                                      message(sprintf(
                                        "[zaro] enriched parquet store: %s (%s), %dx%d, %dx%d tiles, %s+%s",
                                        parsed_md$variable,
                                        parsed_md$dtype,
                                        parsed_md$image_width, parsed_md$image_height,
                                        parsed_md$tile_width, parsed_md$tile_height,
                                        parsed_md$compression,
                                        if (parsed_md$predictor > 0) paste0("predictor=", parsed_md$predictor) else "no predictor"
                                      ))
                                    }

                                    new_object(S7_object(),
                                               path     = path,
                                               metadata = parsed_md,
                                               refs     = ds,
                                               verbose  = verbose
                                    )
                                  }
)

`%||%` <- function(a, b) if (is.null(a)) b else a

# --- Zarr metadata synthesis -------------------------------------------------

#' Synthesize zarr .zarray JSON from enriched parquet metadata
#'
#' This is what makes the enriched parquet work as a zarr store:
#' the .zarray metadata is embedded IN the parquet, not alongside it.
#method(zaro_meta, EnrichedParquetStore) <- function(store, array_name = NULL) {
zaro_meta_enriched <- function(store, array_name = NULL) {
  md <- store@metadata

  # Return a ZaroMeta object (same as zaro produces from real zarr stores)
  new("ZaroMeta",
      zarr_format     = 2L,
      node_type       = "array",
      shape           = md$zarray$shape,
      data_type       = md$dtype,
      chunk_shape     = md$zarray$chunks,
      codecs          = md$zarray$filters,
      fill_value      = md$zarray$fill_value,
      dimension_names = md$zattrs[["_ARRAY_DIMENSIONS"]],
      attributes      = md$zattrs,
      chunk_key_sep   = "."
  )
}


# --- Chunk resolution --------------------------------------------------------

#' Get byte-range reference for a specific chunk
#'
#' @param store EnrichedParquetStore
#' @param chunk_idx integer vector, 0-based chunk indices (time, row, col)
#' @return list(path, offset, length) or NULL if not found
get_chunk_ref <- function(store, chunk_idx) {
  md <- store@metadata

  if (length(chunk_idx) == 3) {
    # 3D: (time, row, col)
    it <- chunk_idx[1]
    iy <- chunk_idx[2]
    ix <- chunk_idx[3]

    # Use time index to filter if available
    # Then match tile_row and tile_col
    ref <- store@refs |>
      dplyr::filter(tile_row == iy, tile_col == ix) |>
      dplyr::collect()

    # If we have multiple files (time dim), select the right one
    if (!is.null(md$dim_time) && nrow(ref) > 1) {
      unique_paths <- unique(ref$path)
      if (it < length(unique_paths)) {
        ref <- ref[ref$path == unique_paths[it + 1], ]
      }
    }
  } else if (length(chunk_idx) == 2) {
    # 2D: (row, col)
    ref <- store@refs |>
      dplyr::filter(tile_row == chunk_idx[1], tile_col == chunk_idx[2]) |>
      dplyr::collect()
  }

  if (nrow(ref) == 0) return(NULL)
  ref <- ref[1, ]  # take first match
  list(path = ref$path, offset = ref$offset, length = ref$length)
}


#' Get all chunk refs within a spatial bbox
#'
#' This is the spatial query — Arrow predicate pushdown filters
#' the parquet without loading non-matching row groups.
get_chunks_bbox <- function(store, xmin, xmax, ymin, ymax,
                            from = NULL, to = NULL) {
  q <- store@refs |>
    dplyr::filter(
      bbox_xmax > xmin,
      bbox_xmin < xmax,
      bbox_ymax > ymin,
      bbox_ymin < ymax
    )

  if (!is.null(from)) q <- q |> dplyr::filter(time >= from)
  if (!is.null(to))   q <- q |> dplyr::filter(time <= to)

  dplyr::collect(q)
}


# --- Pixel decode pipeline ---------------------------------------------------

#' Fetch and decode a single tile from the enriched store
#'
#' This is the zaro equivalent of reading a zarr chunk:
#' fetch bytes, decompress, undo predictor, apply scale/offset.
#'
#' @param store EnrichedParquetStore
#' @param chunk_idx integer vector, 0-based chunk indices
#' @return numeric matrix (tile_h x tile_w)
read_tile <- function(store, chunk_idx) {
  md <- store@metadata

  ref <- get_chunk_ref(store, chunk_idx)
  if (is.null(ref)) {
    # Return fill value tile
    return(matrix(md$zarray$fill_value,
                  nrow = md$tile_height,
                  ncol = md$tile_width))
  }

  # 1. Fetch bytes
  raw_bytes <- fetch_bytes(ref$path, ref$offset, ref$length)

  # 2. Decompress
  codec <- arrow::Codec$create(tolower(md$compression))
  decompressed <- codec$Decompress(
    raw_bytes,
    output_buffer_size = md$tile_width * md$tile_height *
      (md$zarray$chunks |> {\(x) switch(md$dtype,
                                        "<i2" = 2L, "<f4" = 4L, "<f8" = 8L, "|u1" = 1L, 2L)}())
  )

  # 3. Interpret as typed values
  n <- md$tile_width * md$tile_height
  values <- switch(md$dtype,
                   "<i2" = readBin(decompressed, integer(), n = n, size = 2, signed = TRUE,
                                   endian = "little"),
                   "<f4" = readBin(decompressed, double(), n = n, size = 4, endian = "little"),
                   "<f8" = readBin(decompressed, double(), n = n, size = 8, endian = "little"),
                   "|u1" = readBin(decompressed, integer(), n = n, size = 1, signed = FALSE),
                   readBin(decompressed, integer(), n = n, size = 2, signed = TRUE,
                           endian = "little")
  )

  # 4. Reshape to matrix (row-major, as TIFF stores tiles)
  mat <- matrix(values, nrow = md$tile_height, ncol = md$tile_width,
                byrow = TRUE)

  # 5. Undo TIFF predictor (row-wise cumulative sum)
  if (md$predictor == 2L) {
    for (i in seq_len(nrow(mat))) {
      mat[i, ] <- cumsum(mat[i, ])
    }
  }

  # 6. Apply scale/offset (CF convention)
  sf <- md$zattrs$scale_factor %||% 1.0
  ao <- md$zattrs$add_offset %||% 0.0
  fv <- md$zarray$fill_value

  if (!is.null(fv) && !is.na(fv)) {
    mat[mat == fv] <- NA
  }

  mat * sf + ao
}


#' Fetch raw bytes from a URL with byte-range request
#'
#' Uses curl for HTTP(S), arrow for S3/GCS, file connection for local
fetch_bytes <- function(path, offset, length) {
  if (grepl("^https?://", path)) {
    # HTTP byte-range read
    h <- curl::new_handle()
    curl::handle_setheaders(h,
                            Range = sprintf("bytes=%d-%d", offset, offset + length - 1)
    )
    resp <- curl::curl_fetch_memory(path, handle = h)
    resp$content
  } else if (grepl("^s3://", path)) {
    # Arrow S3 byte-range read
    # Parse bucket and key
    parts <- sub("^s3://", "", path)
    bucket <- sub("/.*", "", parts)
    key <- sub("^[^/]+/", "", parts)
    fs <- arrow::S3FileSystem$create(anonymous = TRUE)
    f <- fs$OpenInputFile(path)
    on.exit(f$close())
    f$ReadAt(offset, length)
  } else {
    # Local file
    con <- file(path, "rb")
    on.exit(close(con))
    seek(con, offset)
    readBin(con, raw(), n = length)
  }
}


# --- Multi-tile assembly with vaster -----------------------------------------

#' Read a spatial window from the enriched store
#'
#' Determines which tiles overlap the window, fetches and decodes them,
#' then assembles into a single matrix using tile geometry.
#'
#' @param store EnrichedParquetStore
#' @param xmin,xmax,ymin,ymax numeric, target bounding box
#' @param time_index integer, 0-based time index (for 3D stores)
#' @return numeric matrix
read_window <- function(store, xmin, xmax, ymin, ymax, time_index = 0L) {
  md <- store@metadata
  gt <- md$geotransform

  # Which tiles overlap?
  tile_refs <- get_chunks_bbox(store, xmin, xmax, ymin, ymax)

  if (nrow(tile_refs) == 0) return(NULL)

  # If 3D, filter to the requested time
  if (!is.null(md$dim_time)) {
    target_path <- unique(tile_refs$path)[time_index + 1]
    tile_refs <- tile_refs[tile_refs$path == target_path, ]
  }

  # Compute output grid dimensions
  tw <- md$tile_width
  th <- md$tile_height

  col_range <- range(tile_refs$tile_col)
  row_range <- range(tile_refs$tile_row)
  ncols <- (col_range[2] - col_range[1] + 1) * tw
  nrows <- (row_range[2] - row_range[1] + 1) * th

  # Allocate output
  out <- matrix(NA_real_, nrow = nrows, ncol = ncols)

  # Fetch and place each tile
  for (i in seq_len(nrow(tile_refs))) {
    r <- tile_refs[i, ]
    chunk_idx <- if (!is.null(md$dim_time)) {
      c(time_index, r$tile_row, r$tile_col)
    } else {
      c(r$tile_row, r$tile_col)
    }

    tile_data <- read_tile(store, chunk_idx)

    # Place in output matrix
    row_start <- (r$tile_row - row_range[1]) * th + 1
    col_start <- (r$tile_col - col_range[1]) * tw + 1
    out[row_start:(row_start + th - 1),
        col_start:(col_start + tw - 1)] <- tile_data
  }

  # Crop to actual requested extent (tiles may extend beyond)
  # ... this is where vaster would handle the precise window extraction
  out
}
