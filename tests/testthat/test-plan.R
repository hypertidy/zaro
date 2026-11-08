## a kerchunk V1 JSON store over a local "archival" file: two zlib chunks
## at byte offsets, one inline chunk, one base64 inline chunk, one absent
make_kerchunk <- function() {
  z <- function(v) memCompress(writeBin(as.integer(v), raw(), size = 4L,
                                        endian = "little"), "gzip")
  c00 <- z(c(1, 2, 5, 6)); c01 <- z(c(3, 4, 7, 8))
  src <- tempfile(fileext = ".bin")
  writeBin(c(as.raw(1:7), c00, c01), src)
  c02 <- z(c(9, 10, 13, 14))
  zarray <- paste0('{"zarr_format": 2, "shape": [2, 8], "chunks": [2, 2], ',
                   '"dtype": "<i4", "compressor": {"id": "zlib", "level": 1}, ',
                   '"fill_value": -1, "filters": null, "order": "C"}')
  refs <- list(
    ".zgroup" = '{"zarr_format": 2}',
    "v/.zarray" = zarray,
    "v/.zattrs" = '{"_ARRAY_DIMENSIONS": ["y", "x"]}',
    "v/0.0" = list(src, 7, length(c00)),
    "v/0.1" = list(src, 7 + length(c00), length(c01)),
    "v/0.2" = paste0("base64:", jsonlite::base64_enc(c02))
  )
  json <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(list(version = 1L, refs = refs), auto_unbox = TRUE),
             json)
  list(json = json, src = src, sizes = c(length(c00), length(c01)))
}

test_that("zaro_plan() on a kerchunk store gives byte ranges, inline, absent", {
  k <- make_kerchunk()
  store <- zaro(paste0("reference+json://", k$json), verbose = FALSE)
  p <- zaro_plan(store, "v", verbose = FALSE)
  expect_s3_class(p, "zaro_plan")
  expect_equal(p$key, c("v/0.0", "v/0.1", "v/0.2", "v/0.3"))
  expect_equal(p$transport, c("local", "local", "inline", "absent"))
  expect_equal(p$offset[1:2], c(7, 7 + k$sizes[1]))
  expect_equal(p$length[1:2], k$sizes)
  expect_equal(p$url[1], k$src)
  expect_equal(p$index[[3]], c(0L, 2L))

  ## every row fetches the same bytes store_get() gives
  for (i in seq_len(nrow(p))) {
    expect_identical(zaro:::fetch_ref(store, p[i, ]), store_get(store, p$key[i]))
  }
  ## and the data read through the same path (base64 inline decoded)
  expect_equal(zaro_read(store, "v", verbose = FALSE),
               array(c(1, 5, 2, 6, 3, 7, 4, 8, 9, 13, 10, 14, -1, -1, -1, -1),
                     c(2, 8)), ignore_attr = TRUE)

  s <- zaro:::plan_summary(p)
  expect_equal(s$requests, 2L)
  expect_equal(s$bytes, sum(k$sizes))
  expect_output(print(p), "4 chunk\\(s\\), 2 request\\(s\\)")
})

test_that("zaro_plan() selections and explicit chunk indices", {
  k <- make_kerchunk()
  store <- zaro(paste0("reference+json://", k$json), verbose = FALSE)
  p <- zaro_plan(store, "v", start = c(0, 3), count = c(1, 2), verbose = FALSE)
  expect_equal(p$key, c("v/0.1", "v/0.2"))
  p2 <- zaro_plan(store, "v", chunks = rbind(c(0, 3), c(0, 0)), verbose = FALSE)
  expect_equal(p2$key, c("v/0.3", "v/0.0"))
  expect_error(zaro_plan(store, "v", chunks = rbind(c(0, 4)), verbose = FALSE),
               "outside the chunk grid")
})

test_that("zaro_plan() on a native local store: whole objects, no request", {
  x <- array(seq_len(20) / 2, c(5L, 4L))
  path <- file.path(tempdir(), "plan-native")
  unlink(path, recursive = TRUE)
  write_test_zarr(path, x, chunk = c(2L, 3L), skip = list(c(1L, 1L)))
  store <- zaro(path, verbose = FALSE)
  p <- zaro_plan(store, verbose = FALSE)
  expect_equal(nrow(p), 6L)
  expect_true(all(p$transport == "local"))
  expect_true(all(is.na(p$length)))
  expect_equal(p$url, file.path(path, p$key))
  ## the skipped chunk is planned (existence is unknown until fetched)
  ## and fetches as NULL, like store_get()
  for (i in seq_len(nrow(p))) {
    expect_identical(zaro:::fetch_ref(store, p[i, ]), store_get(store, p$key[i]))
  }
  expect_output(print(p), "6 whole object")
})

test_that("zaro_plan() accepts a zaro_dataset", {
  x <- array(1:6, c(3L, 2L))
  path <- file.path(tempdir(), "plan-ds")
  unlink(path, recursive = TRUE)
  dir.create(path)
  writeLines('{"zarr_format": 2}', file.path(path, ".zgroup"))
  write_test_zarr(file.path(path, "a"), x, chunk = c(3L, 2L), dim_names = c("x", "y"))
  ds <- zaro_dataset(path, verbose = FALSE)
  expect_equal(zaro_plan(ds, "a", verbose = FALSE)$key, "a/0.0")
})
