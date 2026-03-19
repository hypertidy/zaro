# enrich_rustycogs_parquet.R
#
# Take the raw rustycogs tiff_refs() output and enrich it into a
# self-contained, spatiotemporally queryable reference parquet.
#
# The enriched parquet has:
#   1. Precomputed spatial bbox per tile (from geotransform + tile indices)
#   2. Materialized time column (from filename or external source)
#   3. Zarr-compatible metadata as parquet file-level key-value pairs
#   4. Everything needed to decode pixels without touching the source files' metadata
#
# This is the "enriched parquet refs" format — a single file that serves as
# both a byte-range catalogue AND a spatial-temporal index AND a codec spec.

library(arrow)

# --- 1. Schema definition ---------------------------------------------------

#' Build the enriched parquet from rustycogs tiff_refs() output
#'
#' @param refs data.frame from rustycogs::tiff_refs() with columns:
#'   path, ifd, tile_col, tile_row, offset, length, image_w, image_h,
#'   tile_w, tile_h, dtype, compression, bits_per_sample, samples_per_pixel,
#'   crs_epsg
#' @param geotransform numeric(6), GDAL-style geotransform for the grid
#' @param times POSIXct or Date vector, one per source file (in file order)
#' @param time_origin character, e.g. "2002-06-01" for the first file
#' @param variable character, variable name (e.g. "analysed_sst")
#' @param scale_factor numeric, CF scale_factor
#' @param add_offset numeric, CF add_offset
#' @param fill_value numeric, fill/nodata value in raw units
#' @param units character, e.g. "kelvin"
#' @param crs_wkt character, WKT2 string (if not just EPSG)
#' @param predictor integer, TIFF predictor type (0=none, 2=horizontal, 3=float)
#'
#' @return An arrow::Table with enriched columns and file-level metadata
enrich_refs <- function(refs,
                        geotransform,
                        times = NULL,
                        variable = "data",
                        scale_factor = 1.0,
                        add_offset = 0.0,
                        fill_value = NA_real_,
                        units = "",
                        crs_wkt = NULL,
                        predictor = 0L) {

  stopifnot(is.data.frame(refs))
  stopifnot(length(geotransform) == 6)

  gt <- geotransform
  # gt[1] = x_origin, gt[2] = x_res, gt[3] = x_skew
  # gt[4] = y_origin, gt[5] = y_skew, gt[6] = y_res (negative for north-up)

  tw <- refs$tile_w[1]
  th <- refs$tile_h[1]

  # Precompute spatial bbox for each tile
  # tile_col and tile_row are 0-based
  refs$bbox_xmin <- gt[1] + refs$tile_col * tw * gt[2]
  refs$bbox_xmax <- gt[1] + (refs$tile_col + 1) * tw * gt[2]

  # y: gt[6] is typically negative
  refs$bbox_ymax <- gt[4] + refs$tile_row * th * gt[6]
  refs$bbox_ymin <- gt[4] + (refs$tile_row + 1) * th * gt[6]

  # # Ensure ymin < ymax
  # if (gt[6] < 0) {
  #   tmp <- refs$bbox_ymin
  #   refs$bbox_ymin <- refs$bbox_ymax
  #   refs$bbox_ymax <- tmp
  # }

  # Materialize time if provided
  if (!is.null(times)) {
    # Map each file (path) to its time
    unique_paths <- unique(refs$path)
    if (length(times) != length(unique_paths)) {
      stop("length(times) must equal the number of unique source files")
    }
    time_lookup <- stats::setNames(times, unique_paths)
    refs$time <- time_lookup[refs$path]
  }

  # Convert to Arrow Table
  tbl <- arrow::as_arrow_table(refs)

  # --- 2. File-level metadata (key-value pairs on the parquet schema) --------

  # Build zarr .zarray equivalent
  zarray <- jsonlite::toJSON(list(
    zarr_format = 2L,
    shape = as.integer(c(
      if (!is.null(times)) length(unique(refs$path)) else 1L,
      # Round up to full tile multiples (TIFF always stores full tiles)
      ceiling(refs$image_h[1] / th) * th,
      ceiling(refs$image_w[1] / tw) * tw
    )),
    chunks = as.integer(c(1L, th, tw)),
    dtype = refs$dtype[1],
    compressor = jsonlite::toJSON(list(
      id = tolower(refs$compression[1]),
      level = jsonlite::unbox(NA)
    ), auto_unbox = TRUE),
    fill_value = jsonlite::unbox(fill_value),
    order = jsonlite::unbox("C"),
    filters = if (predictor > 0) {
      list(list(
        id = jsonlite::unbox("tiff_predictor"),
        predictor = jsonlite::unbox(predictor),
        tilewidth = jsonlite::unbox(as.integer(tw)),
        nbits = jsonlite::unbox(as.integer(refs$bits_per_sample[1]))
      ))
    } else {
      list()
    }
  ), auto_unbox = FALSE, null = "null")

  # Build zarr .zattrs equivalent
  zattrs <- jsonlite::toJSON(list(
    `_ARRAY_DIMENSIONS` = if (!is.null(times)) c("time", "y", "x") else c("y", "x"),
    scale_factor = jsonlite::unbox(scale_factor),
    add_offset = jsonlite::unbox(add_offset),
    units = jsonlite::unbox(units),
    `_FillValue` = jsonlite::unbox(fill_value)
  ), auto_unbox = FALSE)

  # CRS
  if (is.null(crs_wkt)) {
    crs_wkt <- if (!is.na(refs$crs_epsg[1])) {
      sprintf("EPSG:%d", refs$crs_epsg[1])
    } else {
      ""
    }
  }

  # Assemble all metadata
  metadata <- list(
    variable = variable,
    geotransform = paste(geotransform, collapse = ","),
    crs_wkt = crs_wkt,
    zarray = as.character(zarray),
    zattrs = as.character(zattrs),
    image_width = as.character(refs$image_w[1]),
    image_height = as.character(refs$image_h[1]),
    tile_width = as.character(tw),
    tile_height = as.character(th),
    dtype = refs$dtype[1],
    compression = refs$compression[1],
    predictor = as.character(predictor),
    bits_per_sample = as.character(refs$bits_per_sample[1]),
    samples_per_pixel = as.character(refs$samples_per_pixel[1]),
    n_files = as.character(length(unique(refs$path))),
    format_version = "enriched-parquet-refs/0.1"
  )

  # Embed time coordinate array if present
  if (!is.null(times)) {
    metadata$dim_time <- paste(as.character(times), collapse = ",")
  }

  tbl$metadata <- metadata
  tbl
}


#' Write enriched parquet
write_enriched_parquet <- function(tbl, path) {
  arrow::write_parquet(tbl, path)
  message(sprintf("[enriched-refs] wrote %d rows to %s", nrow(tbl), path))
  invisible(path)
}


#' Read enriched parquet and return both data and metadata
read_enriched_parquet <- function(path) {
  tbl <- arrow::read_parquet(path, as_data_frame = FALSE)
  schema <- arrow::open_dataset(path)$schema

  # Extract file-level metadata from the Arrow schema
  md <- schema$metadata
  # arrow stores metadata as named character vector
  # the "enriched" keys are mixed in with arrow/pandas metadata

  list(
    refs = dplyr::collect(tbl),
    metadata = md
  )
}


# --- 3. Example: GHRSST MUR enrichment --------------------------------------

#' Enrich GHRSST MUR v4.1 references
#'
#' This is the specific enrichment for the GHRSST dataset we've been working with.
#' Geotransform and CF attributes are known constants for this product.
enrich_ghrsst_mur <- function(refs) {

  # GHRSST MUR v4.1 constants
  # 0.01 degree resolution, global coverage
  # Longitude: -180 to 180, Latitude: -90 to 90
  #gt <- c(-180.0, 0.01, 0, 90.0, 0, -0.01)
  gt <- c(-179.995, 0.01, 0, 89.995, 0, -0.01)

  # Extract dates from filenames
  # Pattern: YYYYMMDD in the filename
  dates <- stringr::str_extract(unique(refs$path), "\\d{8}(?=\\d{6})")
  times <- as.Date(dates, format = "%Y%m%d")

  enrich_refs(
    refs = refs,
    geotransform = gt,
    times = times,
    variable = "analysed_sst",
    scale_factor = 0.001,
    add_offset = 25.0,  # CHECK: might be 298.15 for some versions
    fill_value = -32768,
    units = "kelvin",
    predictor = 2L
  )
}


# --- 4. Spatial subsetting on enriched parquet (Arrow-native) ----------------

#' Subset enriched refs by bounding box
#'
#' Uses Arrow predicate pushdown — no data loaded for non-matching row groups.
#' Returns a lazy arrow query or collected data.frame.
subset_bbox <- function(path, xmin, xmax, ymin, ymax, collect = TRUE) {
  ds <- arrow::open_dataset(path)
  q <- ds |>
    dplyr::filter(
      bbox_xmax > xmin,
      bbox_xmin < xmax,
      bbox_ymax > ymin,
      bbox_ymin < ymax
    )
  if (collect) dplyr::collect(q) else q
}


#' Subset enriched refs by time range
subset_time <- function(path, from, to, collect = TRUE) {
  ds <- arrow::open_dataset(path)
  q <- ds |>
    dplyr::filter(time >= from, time <= to)
  if (collect) dplyr::collect(q) else q
}


#' Subset by both space and time
subset_spatiotemporal <- function(path, xmin, xmax, ymin, ymax,
                                  from = NULL, to = NULL,
                                  collect = TRUE) {
  ds <- arrow::open_dataset(path)
  q <- ds |>
    dplyr::filter(
      bbox_xmax > xmin,
      bbox_xmin < xmax,
      bbox_ymax > ymin,
      bbox_ymin < ymax
    )
  if (!is.null(from)) q <- q |> dplyr::filter(time >= from)
  if (!is.null(to))   q <- q |> dplyr::filter(time <= to)
  if (collect) dplyr::collect(q) else q
}
