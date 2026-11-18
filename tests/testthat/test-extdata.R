## inst/extdata/example.zarr, the store the help page examples read
## (written by data-raw/example_zarr.py with zarr-python 3.2.1)

## drop the dimension names (zaro_read() names dim())
bare <- function(x) {
  dim(x) <- unname(dim(x))
  x
}

test_that("the example store reads as zarr-python wrote it", {
  f <- system.file("extdata", "example.zarr", package = "zaro")
  st <- zaro(f, verbose = FALSE)
  expect_equal(zaro_groups(st, verbose = FALSE)$group, c("", "detail"))

  lon <- seq(140.25, 147.75, length.out = 16)
  lat <- seq(-38.25, -41.75, length.out = 8)
  expect_equal(as.vector(zaro_read(st, "lon", verbose = FALSE)), lon)
  expect_equal(as.vector(zaro_read(st, "lat", verbose = FALSE)), lat)
  expect_identical(as.vector(zaro_read(st, "time", verbose = FALSE)), 0:2)

  temp <- zaro_read(st, "temp", verbose = FALSE)
  expect_equal(dim(temp), c(time = 3L, lat = 8L, lon = 16L))
  ref <- array(NA_real_, c(3, 8, 16))
  for (t in 1:3) ref[t, , ] <- 15 + 0.5 * (lat + 40) +
    outer(rep(1, 8), 0.2 * (lon - 144)) + (t - 1)
  ref[1, 1, 1:3] <- NaN
  expect_equal(bare(temp), ref, tolerance = 1e-6)   # float32

  ## sharded, in a nested group
  elev <- zaro_read(st, "detail/elev", verbose = FALSE)
  expect_identical(bare(elev), outer(0:15 * 100L, 0:15, "+"))
  win <- zaro_read(st, "detail/elev", start = c(6, 7), count = c(3, 2),
                   verbose = FALSE)
  expect_identical(bare(win), outer(6:8 * 100L, 7:8, "+"))
})
