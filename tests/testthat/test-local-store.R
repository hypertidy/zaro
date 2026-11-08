## Local stores are read with base R (no arrow)

test_that("zaro() on a local path gives a LocalStore with base-R methods", {
  d <- file.path(tempdir(), "local-store.zarr")
  unlink(d, recursive = TRUE)
  dir.create(file.path(d, "g", "a"), recursive = TRUE)
  writeLines('{"zarr_format": 2}', file.path(d, ".zgroup"))
  writeLines('{"zarr_format": 2}', file.path(d, "g", ".zgroup"))
  writeBin(as.raw(0:9), file.path(d, "g", "a", "0"))

  st <- zaro(d, verbose = FALSE)
  expect_true(S7::S7_inherits(st, LocalStore))

  expect_setequal(store_list(st), c(".zgroup", "g"))
  expect_setequal(store_list(st, "g"), c("g/.zgroup", "g/a"))
  expect_setequal(store_list(st, "g/", recursive = TRUE),
                  c("g/.zgroup", "g/a", "g/a/0"))
  expect_length(store_list(st, "nope"), 0L)

  expect_identical(store_get(st, "g/a/0"), as.raw(0:9))
  expect_null(store_get(st, "g/a"))            # a directory
  expect_null(store_get(st, "missing"))
  expect_true(store_exists(st, "g/.zgroup"))
  expect_false(store_exists(st, "missing"))
  expect_identical(store_range(st, "g/a/0", 2, 3), as.raw(2:4))
  expect_identical(store_range(st, "g/a/0", -1, 2), as.raw(8:9))

  r <- store_refs(st, "g/a/0")
  expect_identical(r$transport, "local")
})

test_that("cloud URLs map to GDAL VSI paths", {
  expect_identical(cloud_to_vsi("s3://b/k/x.zarr"), "/vsis3/b/k/x.zarr")
  expect_identical(cloud_to_vsi("gs://b/k"), "/vsigs/b/k")
})
