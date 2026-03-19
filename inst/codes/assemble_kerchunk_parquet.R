#!/usr/bin/env Rscript
## assemble_kerchunk_parquet.R
##
## Build a kerchunk parquet reference store from rustycogs::tiff_refs()
## for GHRSST MUR v4.1 COGs on Source Cooperative.
##
## Output directory layout (consumed by GDAL >= 3.11 Zarr driver):
##
##   .zmetadata                     # consolidated zarr v2 metadata
##   analysed_sst/refs.0.parq       # chunk references (one big partition)
##
## No Python required. All metadata constructed from known COG properties.
##
## Usage:
##   source("assemble_kerchunk_parquet.R")
##   test_one_file()                          # quick sanity check
##   build_ghrsst_store("2002-06-01", "2002-06-30", "ghrsst_june2002.parq")

library(arrow)
library(bit64)
library(jsonlite)

# ---------------------------------------------------------------------------
# COG properties (from gdalinfo on any GHRSST MUR file)
# ---------------------------------------------------------------------------
# COMPRESSION=ZSTD, PREDICTOR=2
# Band 1 Block=512x512 Type=Int16, NoData=-32768
# Offset: 25, Scale: 0.001, Unit: celsius
# 36000 x 18000 pixels, 0.01 degree
# Origin: (-179.995, 89.995)

GHRSST_META <- list(
  image_width     = 36000L,
  image_height    = 18000L,
  tile_width      = 512L,
  tile_height     = 512L,
  n_tiles_x       = 71L,    # ceiling(36000/512)
  n_tiles_y       = 36L,    # ceiling(18000/512)  (last row = 80px)
  tiles_per_file  = 2556L,  # 71 * 36
  dtype           = "<i2",  # Int16 little-endian
  fill_value      = -32768L,
  compression     = "zstd",
  predictor       = 2L,
  scale_factor    = 0.001,
  add_offset      = 25.0,
  units           = "celsius",
  origin_x        = -179.995,
  origin_y        = 89.995,
  pixel_size_x    = 0.01,
  pixel_size_y    = -0.01
)

# ---------------------------------------------------------------------------
# URL generation
# ---------------------------------------------------------------------------

ghrsst_urls <- function(start_date, end_date,
                        base = "https://data.source.coop/ausantarctic/ghrsst-mur-v2") {
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  vapply(dates, function(d) {
    sprintf("%s/%04d/%02d/%02d/%s090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-v02.0-fv04.1_analysed_sst.tif",
            base,
            as.integer(format(d, "%Y")),
            as.integer(format(d, "%m")),
            as.integer(format(d, "%d")),
            format(d, "%Y%m%d"))
  }, character(1))
}

# ---------------------------------------------------------------------------
# Collect refs from rustycogs
# ---------------------------------------------------------------------------

collect_refs <- function(urls, region = "", anon = TRUE,
                         concurrency = 15L) {
  refs_list <- vector("list", length(urls))

  for (i in seq_along(urls)) {
    if (i %% 50 == 1 || i == length(urls)) {
      message(sprintf("  scanning %d/%d ...", i, length(urls)))
    }

    r <- rustycogs::tiff_refs(urls[i], region = region,
                              anon = anon,
                              concurrency = concurrency)

    ## full resolution only (ifd == 0), skip overviews
    r_full <- r[r$ifd == 0, ]

    ## C-order sort: row varies slower, col varies faster
    r_full <- r_full[order(r_full$tile_row, r_full$tile_col), ]

    if (i == 1L) {
      stopifnot(
        "unexpected tile count" = nrow(r_full) == GHRSST_META$tiles_per_file
      )
      message(sprintf("  tile grid: %d cols x %d rows = %d tiles/file",
                      max(r_full$tile_col) + 1L,
                      max(r_full$tile_row) + 1L,
                      nrow(r_full)))
    }

    refs_list[[i]] <- data.frame(
      path   = r_full$path,
      offset = r_full$offset,
      size   = r_full$length,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, refs_list)
}

# ---------------------------------------------------------------------------
# Build .zmetadata (pure R — no VirtualiZarr needed)
# ---------------------------------------------------------------------------
#
# Compressor mode controls what we tell GDAL about the tile encoding:
#
#   "A" — zstd compressor + delta filter (full zarr v2 translation)
#   "B" — zstd compressor only, no filter (GDAL may handle predictor)
#   "C" — null compressor, null filter (GDAL handles everything)
#
# Start with "B", compare pixel values, adjust if wrong.

build_zmetadata <- function(n_times, dates, compressor_mode = "B") {
  m <- GHRSST_META

  ## Use NA (not NULL) for JSON null — R drops NULL from lists
  compressor <- switch(compressor_mode,
                       "A" = list(id = "zstd", level = 9L),
                       "B" = list(id = "zstd", level = 9L),
                       "C" = NA
  )

  filters <- switch(compressor_mode,
                    "A" = list(list(id = "delta", dtype = "<i2")),
                    "B" = NA,
                    "C" = NA
  )

  zarray_sst <- list(
    zarr_format = 2L,
    shape       = c(n_times, m$image_height, m$image_width),
    chunks      = c(1L, m$tile_height, m$tile_width),
    dtype       = m$dtype,
    fill_value  = m$fill_value,
    order       = "C",
    compressor  = compressor,
    filters     = filters
  )

  zattrs_sst <- list(
    `_ARRAY_DIMENSIONS` = c("time", "y", "x"),
    scale_factor        = m$scale_factor,
    add_offset          = m$add_offset,
    units               = m$units,
    long_name           = "analysed sea surface temperature",
    `_FillValue`        = m$fill_value
  )

  ## time coordinate
  time_values <- as.numeric(dates - as.Date("1981-01-01"))

  time_zarray <- list(
    zarr_format = 2L,
    shape       = list(length(time_values)),
    chunks      = list(length(time_values)),
    dtype       = "<f8",
    fill_value  = 0.0,
    order       = "C",
    compressor  = NA,
    filters     = NA
  )

  time_zattrs <- list(
    `_ARRAY_DIMENSIONS` = list("time"),
    units     = "days since 1981-01-01",
    calendar  = "proleptic_gregorian"
  )

  ## GDAL expects metadata values as JSON objects, not strings
  metadata <- list(
    `.zgroup`              = list(zarr_format = 2L),
    `.zattrs`              = setNames(list(), character(0)),
    `analysed_sst/.zarray` = zarray_sst,
    `analysed_sst/.zattrs` = zattrs_sst,
    `time/.zarray`         = time_zarray,
    `time/.zattrs`         = time_zattrs
  )

  list(
    metadata    = metadata,
    time_values = time_values
  )
}

# ---------------------------------------------------------------------------
# Write inline coordinate data
# ---------------------------------------------------------------------------

write_coord_parquet <- function(values, varname, output_dir) {
  dir.create(file.path(output_dir, varname), showWarnings = FALSE)
  raw_bytes <- writeBin(values, raw(), size = 8, endian = "little")

  tbl <- arrow::arrow_table(
    path   = NA_character_,
    offset = bit64::as.integer64(0),
    size   = bit64::as.integer64(0),
    raw    = arrow::Array$create(list(raw_bytes), type = arrow::binary())
  )

  arrow::write_parquet(tbl,
                       file.path(output_dir, varname, "refs.0.parq"),
                       compression = "snappy")
}

# ---------------------------------------------------------------------------
# Write the store
# ---------------------------------------------------------------------------

write_kerchunk_parquet <- function(refs, zmeta, output_dir, record_size = NULL) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "analysed_sst"), showWarnings = FALSE)

  n_refs <- nrow(refs)
  if (is.null(record_size)) record_size <- n_refs

  ## .zmetadata
  writeLines(
    toJSON(list(metadata = zmeta$metadata, record_size = record_size),
           auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"),
    file.path(output_dir, ".zmetadata")
  )

  ## refs parquet — one big partition
  ## raw column must be binary type with null values (not list<null>)
  raw_col <- arrow::Array$create(
    rep(list(NULL), n_refs),
    type = arrow::binary()
  )

  refs_tbl <- arrow::arrow_table(
    path   = refs$path,
    offset = bit64::as.integer64(refs$offset),
    size   = bit64::as.integer64(refs$size),
    raw    = raw_col
  )

  arrow::write_parquet(refs_tbl,
                       file.path(output_dir, "analysed_sst", "refs.0.parq"),
                       compression = "snappy")

  ## time coordinate (inline)
  write_coord_parquet(zmeta$time_values, "time", output_dir)

  message(sprintf("Wrote %d refs (%d files x %d tiles) to %s",
                  n_refs,
                  n_refs / GHRSST_META$tiles_per_file,
                  GHRSST_META$tiles_per_file,
                  output_dir))
  invisible(output_dir)
}

# ---------------------------------------------------------------------------
# Main entry point
# ---------------------------------------------------------------------------

build_ghrsst_store <- function(start_date = "2002-06-01",
                               end_date   = "2002-06-30",
                               output_dir = "ghrsst_mur_kerchunk.parq",
                               compressor_mode = "B",
                               region = "",
                               concurrency = 15L) {

  urls <- ghrsst_urls(start_date, end_date)
  dates <- as.Date(
    stringr::str_extract(basename(urls), "^\\d{8}"),
    format = "%Y%m%d")

  message(sprintf("Scanning %d files ...", length(urls)))
  refs <- collect_refs(urls, region = region, concurrency = concurrency)

  message(sprintf("Building .zmetadata (mode=%s) ...", compressor_mode))
  zmeta <- build_zmetadata(length(urls), dates, compressor_mode)

  write_kerchunk_parquet(refs, zmeta, output_dir)

  message("\nTest with:")
  message(sprintf('  gdalmdiminfo "%s"', output_dir))
  invisible(output_dir)
}

# ---------------------------------------------------------------------------
# Quick single-file test (2D, no time dim) for debugging compressor mode
# ---------------------------------------------------------------------------

test_one_file <- function(url = NULL,
                          output_dir = "test_one.parq",
                          compressor_mode = "B") {
  if (is.null(url)) {
    url <- paste0(
      "https://data.source.coop/ausantarctic/ghrsst-mur-v2/",
      "2002/06/01/20020601090000-JPL-L4_GHRSST-SSTfnd-MUR-GLOB-",
      "v02.0-fv04.1_analysed_sst.tif")
  }

  message("Scanning single file ...")
  refs <- collect_refs(url, region = "", concurrency = 15L)
  m <- GHRSST_META

  compressor <- switch(compressor_mode,
                       "A" = list(id = "zstd", level = 9L),
                       "B" = list(id = "zstd", level = 9L),
                       "C" = NA)
  filters <- switch(compressor_mode,
                    "A" = list(list(id = "delta", dtype = "<i2")),
                    "B" = NA,
                    "C" = NA)

  ## 2D zarray — simplest case
  zarray <- list(
    zarr_format = 2L,
    shape       = c(m$image_height, m$image_width),
    chunks      = c(m$tile_height, m$tile_width),
    dtype       = m$dtype,
    fill_value  = m$fill_value,
    order       = "C",
    compressor  = compressor,
    filters     = filters)

  zattrs <- list(
    `_ARRAY_DIMENSIONS` = c("y", "x"),
    scale_factor        = m$scale_factor,
    add_offset          = m$add_offset,
    units               = m$units,
    `_FillValue`        = m$fill_value)

  metadata <- list(
    `.zgroup`              = list(zarr_format = 2L),
    `.zattrs`              = setNames(list(), character(0)),
    `analysed_sst/.zarray` = zarray,
    `analysed_sst/.zattrs` = zattrs)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "analysed_sst"), showWarnings = FALSE)

  n <- nrow(refs)
  writeLines(
    toJSON(list(metadata = metadata, record_size = n),
           auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"),
    file.path(output_dir, ".zmetadata"))

  arrow::write_parquet(
    arrow::arrow_table(
      path   = refs$path,
      offset = bit64::as.integer64(refs$offset),
      size   = bit64::as.integer64(refs$size),
      raw    = arrow::Array$create(rep(list(NULL), n), type = arrow::binary())),
    file.path(output_dir, "analysed_sst", "refs.0.parq"),
    compression = "snappy")

  message(sprintf("\nWrote %d refs to %s (mode=%s)", n, output_dir,
                  compressor_mode))
  message("\n--- Validation ---")
  message(sprintf('gdalmdiminfo "%s"', output_dir))
  message("")
  message("## Compare pixel values (top-left tile):")
  message(sprintf('direct <- vapour::vapour_read_raster("%s",', url))
  message('  window = c(0, 0, 512, 512))')
  message(sprintf('zarr <- vapour::vapour_read_raster('))
  message(sprintf('  \'ZARR:"%s":/analysed_sst\',', output_dir))
  message('  window = c(0, 0, 512, 512))')
  message('identical(direct[[1]], zarr[[1]])')
  message("")
  message('## If wrong, try other modes:')
  message('test_one_file(compressor_mode = "A")  # zstd + delta filter')
  message('test_one_file(compressor_mode = "C")  # null (GDAL handles all)')

  invisible(output_dir)
}
