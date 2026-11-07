## a small xarray-like V2 store: 1D coords lon/lat/time plus a packed int
## "temp" (time, lat, lon) and an unpacked double "mask" (lat, lon)
make_store <- function(path, consolidate = FALSE) {
  unlink(path, recursive = TRUE)
  dir.create(path, recursive = TRUE)
  writeLines('{"zarr_format": 2}', file.path(path, ".zgroup"))
  writeLines('{"title": "test store"}', file.path(path, ".zattrs"))
  lon <- array(seq(100.5, by = 1, length.out = 6), 6)
  lat <- array(seq(-50.5, by = 1, length.out = 4), 4)
  tim <- array(c(0, 1, 2), 3)
  write_test_zarr(file.path(path, "lon"), lon, chunk = 6L, dim_names = "lon")
  write_test_zarr(file.path(path, "lat"), lat, chunk = 4L, dim_names = "lat")
  write_test_zarr(file.path(path, "time"), tim, chunk = 3L, dim_names = "time")
  temp <- array(seq_len(6 * 4 * 3), c(6L, 4L, 3L))
  temp[2, 3, 1] <- -999L
  write_test_zarr(file.path(path, "temp"), temp, chunk = c(4L, 3L, 1L),
                  fill = -999, dim_names = c("lon", "lat", "time"))
  writeLines(paste0('{"_ARRAY_DIMENSIONS": ["time", "lat", "lon"], ',
                    '"scale_factor": 0.5, "add_offset": 10, ',
                    '"_FillValue": -999, "units": "degC"}'),
             file.path(path, "temp", ".zattrs"))
  mask <- array(as.double(seq_len(24)), c(6L, 4L))
  write_test_zarr(file.path(path, "mask"), mask, chunk = c(6L, 4L),
                  fill = "NaN", dim_names = c("lon", "lat"))
  time_attrs <- '{"_ARRAY_DIMENSIONS": ["time"], "units": "days since 2020-01-01"}'
  writeLines(time_attrs, file.path(path, "time", ".zattrs"))

  if (consolidate) {
    files <- list.files(path, pattern = "^\\.z", recursive = TRUE,
                        all.files = TRUE)
    ## assembled as text: the entries keep Python's bare NaN, as real
    ## .zmetadata files do
    entries <- vapply(files, function(f) {
      sprintf('"%s": %s', f, paste(readLines(file.path(path, f)), collapse = ""))
    }, character(1))
    writeLines(sprintf('{"zarr_consolidated_format": 1, "metadata": {%s}}',
                       paste(entries, collapse = ", ")),
               file.path(path, ".zmetadata"))
  }
  list(path = path, temp = temp, lon = lon, mask = mask)
}

test_that("zaro_dataset lists arrays and R-order dims, listing a store", {
  s <- make_store(file.path(tempdir(), "ds-listed"))
  ds <- zaro_dataset(s$path, verbose = FALSE)
  expect_s3_class(ds, "zaro_dataset")
  expect_setequal(names(ds$arrays), c("lat", "lon", "mask", "temp", "time"))
  expect_equal(ds$dims$temp, c("lon", "lat", "time"))
  expect_equal(ds$shape$temp, c(lon = 6L, lat = 4L, time = 3L))
  expect_equal(ds$attributes$title, "test store")
  expect_output(print(ds), "5 array")
})

test_that("zaro_dataset uses consolidated metadata, and arrays= filters", {
  s <- make_store(file.path(tempdir(), "ds-consolidated"), consolidate = TRUE)
  ds <- zaro_dataset(s$path, verbose = FALSE)
  expect_setequal(names(ds$arrays), c("lat", "lon", "mask", "temp", "time"))
  expect_equal(ds$dims$mask, c("lon", "lat"))

  ds2 <- zaro_dataset(s$path, arrays = c("temp", "lon"), verbose = FALSE)
  expect_equal(names(ds2$arrays), c("temp", "lon"))
  expect_error(zaro_dataset(s$path, arrays = "nope", verbose = FALSE),
               "not found")
})

test_that("zaro_array takes a dataset; cf = TRUE masks and unpacks", {
  skip_if_not_installed("altarr")
  s <- make_store(file.path(tempdir(), "ds-cf"), consolidate = TRUE)
  ds <- zaro_dataset(s$path, verbose = FALSE)

  raw <- zaro_array(ds, "temp", verbose = FALSE)
  expect_identical(typeof(raw), "integer")
  expect_identical(raw[seq_along(raw)], as.vector(s$temp))

  dec <- zaro_array(ds, "temp", cf = TRUE, verbose = FALSE)
  expect_identical(typeof(dec), "double")
  want <- as.vector(s$temp) * 0.5 + 10
  want[as.vector(s$temp) == -999L] <- NA
  expect_equal(dec[seq_along(dec)], want)

  ## nothing to unpack: type kept, NaN fill reads NA
  m <- zaro_array(ds, "mask", cf = TRUE, verbose = FALSE)
  expect_identical(typeof(m), "double")
  expect_equal(m[seq_along(m)], as.vector(s$mask))

  expect_equal(as.vector(zaro_array(ds, "lon", verbose = FALSE)[1:6]),
               as.vector(s$lon))
  expect_error(zaro_array(ds, "nope", verbose = FALSE), "not in the dataset")
})

test_that("missing chunks read NA under cf = TRUE", {
  skip_if_not_installed("altarr")
  path <- file.path(tempdir(), "ds-missing")
  unlink(path, recursive = TRUE)
  x <- array(1:20, c(5L, 4L))
  write_test_zarr(path, x, chunk = c(2L, 3L), compressor = "none",
                  fill = -9, skip = list(c(1L, 1L)))
  a <- zaro_array(zaro(path, verbose = FALSE), cf = TRUE, verbose = FALSE)
  y <- x
  y[3:4, 4] <- NA
  expect_identical(a[seq_along(a)], as.vector(y))
})

test_that("arrays without dimension names get unique names", {
  path <- file.path(tempdir(), "ds-nodims")
  unlink(path, recursive = TRUE)
  dir.create(path)
  writeLines('{"zarr_format": 2}', file.path(path, ".zgroup"))
  write_test_zarr(file.path(path, "a"), array(1, c(2L, 3L)), chunk = c(2L, 3L))
  ds <- zaro_dataset(path, verbose = FALSE)
  expect_equal(ds$dims$a, c("a_dim1", "a_dim0"))
})

test_that("no copy of print leaks into the namespace (S3 print dispatch)", {
  ## a namespace binding named `print` captures S3method(print, ...) and
  ## user code then never dispatches print.zaro_dataset
  expect_false(exists("print", envir = asNamespace("zaro"), inherits = FALSE))
})

test_that("consolidated metadata keeps full numeric precision (V2 and V3)", {
  ## BRAN2023 temp's scale_factor; jsonlite's default digits = 4 made it 0.0078
  sf <- 0.007782219909131527
  fv <- 9.969209968386869e+36
  path <- file.path(tempdir(), "ds-precision-v2")
  unlink(path, recursive = TRUE)
  dir.create(path)
  writeLines(sprintf(paste0(
    '{"zarr_consolidated_format": 1, "metadata": {".zgroup": {"zarr_format": 2}, ',
    '".zattrs": {"g": 0.1234567890123456}, ',
    '"a/.zarray": {"zarr_format": 2, "shape": [2], "chunks": [2], "dtype": "<f8", ',
    '"compressor": null, "fill_value": %.17g, "filters": null, "order": "C"}, ',
    '"a/.zattrs": {"_ARRAY_DIMENSIONS": ["x"], "scale_factor": %.17g}}}'), fv, sf),
    file.path(path, ".zmetadata"))
  ds <- zaro_dataset(path, verbose = FALSE)
  expect_identical(ds$arrays$a@attributes$scale_factor, sf)
  expect_identical(ds$arrays$a@fill_value, fv)
  expect_identical(ds$attributes$g, 0.1234567890123456)

  path3 <- file.path(tempdir(), "ds-precision-v3")
  unlink(path3, recursive = TRUE)
  dir.create(path3)
  writeLines(sprintf(paste0(
    '{"zarr_format": 3, "node_type": "group", "attributes": {}, ',
    '"consolidated_metadata": {"kind": "inline", "must_understand": false, "metadata": {',
    '"a": {"zarr_format": 3, "node_type": "array", "shape": [2], "data_type": "float64", ',
    '"chunk_grid": {"name": "regular", "configuration": {"chunk_shape": [2]}}, ',
    '"chunk_key_encoding": {"name": "default", "configuration": {"separator": "/"}}, ',
    '"codecs": [{"name": "bytes", "configuration": {"endian": "little"}}], ',
    '"fill_value": %.17g, "dimension_names": ["x"], ',
    '"attributes": {"scale_factor": %.17g}}}}}'), fv, sf),
    file.path(path3, "zarr.json"))
  ds3 <- zaro_dataset(path3, verbose = FALSE)
  expect_identical(ds3$arrays$a@attributes$scale_factor, sf)
  expect_identical(ds3$arrays$a@fill_value, fv)
})

test_that("options(zaro.verbose = FALSE) silences zaro by default", {
  path <- file.path(tempdir(), "ds-quiet")
  unlink(path, recursive = TRUE)
  x <- array(1, c(2L, 2L))
  write_test_zarr(path, x, chunk = c(2L, 2L))
  msgs <- capture_messages(zaro_meta(zaro(path), verbose = TRUE))
  expect_true(any(grepl("[zaro]", msgs, fixed = TRUE)))
  old <- options(zaro.verbose = FALSE)
  on.exit(options(old), add = TRUE)
  expect_silent(zaro_read(zaro(path)))
  expect_silent(zaro_dataset(path))
  ## an explicit argument still wins
  msgs <- capture_messages(zaro(path, verbose = TRUE))
  expect_true(any(grepl("[zaro]", msgs, fixed = TRUE)))
})
