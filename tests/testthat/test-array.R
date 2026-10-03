skip_if_not_installed("altarr")

make_x <- function(d, type = "double") {
  v <- seq_len(prod(d))
  if (type == "double") v <- v / 10
  array(v, d)
}

test_that("zaro_array matches the source for a C-order V2 store", {
  x <- make_x(c(7L, 5L, 3L))
  path <- file.path(tempdir(), "zarr-v2-c")
  write_test_zarr(path, x, chunk = c(3L, 2L, 2L),
                  dim_names = c("lon", "lat", "time"))
  store <- zaro(path, verbose = FALSE)
  a <- zaro_array(store, verbose = FALSE)

  expect_true(altarr::is_altarr(a))
  expect_equal(dim(a), dim(x))
  expect_equal(names(dimnames(a)), c("lon", "lat", "time"))
  expect_equal(altarr::altarr_stats(a)[["fetch_calls"]], 0)

  ## planned reads
  expect_equal(a[c(1, 17, 105)], x[c(1, 17, 105)])
  m <- cbind(c(1, 7, 4), c(1, 5, 3), c(1, 3, 2))
  expect_equal(a[m], x[m])
  expect_equal(altarr::altarr_extract(a, 2:7, 4:5, 3),
               unname(x[2:7, 4:5, 3]))
  expect_equal(sum(a), sum(x))

  ## zaro_read() returns the Zarr order, the transpose
  r <- zaro_read(store, verbose = FALSE)
  expect_equal(as.vector(aperm(unname(r))), as.vector(x))
})

test_that("missing chunks read as fill_value, integer type kept", {
  x <- make_x(c(5L, 4L), type = "integer")
  path <- file.path(tempdir(), "zarr-v2-missing")
  write_test_zarr(path, x, chunk = c(2L, 3L), compressor = "none",
                  fill = -9, skip = list(c(1L, 1L)))
  a <- zaro_array(zaro(path, verbose = FALSE), verbose = FALSE)
  expect_identical(typeof(a), "integer")
  y <- x
  y[3:4, 4] <- -9L
  expect_identical(a[seq_along(a)], as.vector(y))
})

test_that("F-order V2 keeps the Zarr shape", {
  x <- make_x(c(4L, 6L))
  path <- file.path(tempdir(), "zarr-v2-f")
  write_test_zarr(path, x, chunk = c(3L, 4L), order = "F")
  a <- zaro_array(zaro(path, verbose = FALSE), verbose = FALSE)
  expect_equal(dim(a), c(4L, 6L))
  expect_equal(a[seq_along(a)], as.vector(x))
})

test_that("V3 store with dimension_names", {
  x <- make_x(c(6L, 4L, 2L))
  path <- file.path(tempdir(), "zarr-v3")
  write_test_zarr(path, x, chunk = c(4L, 3L, 1L), version = 3L,
                  dim_names = c("x", "y", "band"))
  a <- zaro_array(zaro(path, verbose = FALSE), verbose = FALSE)
  expect_equal(dim(a), dim(x))
  expect_equal(names(dimnames(a)), c("x", "y", "band"))
  expect_equal(a[seq_along(a)], as.vector(x))
  expect_equal(max(a), max(x))
})
