skip_if_not_installed("curl")

## consolidate a local V2 store's metadata into .zmetadata (text, as Python does)
consolidate <- function(path) {
  files <- list.files(path, pattern = "^\\.z", recursive = TRUE, all.files = TRUE)
  entries <- vapply(files, function(f) {
    sprintf('"%s": %s', f, paste(readLines(file.path(path, f)), collapse = ""))
  }, character(1))
  writeLines(sprintf('{"zarr_consolidated_format": 1, "metadata": {%s}}',
                     paste(entries, collapse = ", ")), file.path(path, ".zmetadata"))
}

test_that("http_requests() merges touching ranges per URL, within limits", {
  url <- c("a", "a", "a", "b", "a", "c")
  off <- c(0, 10, 25, 0, 40, 0)
  len <- c(10, 10, 5, 4, 5, NA)
  r <- http_requests(url, off, len)
  ## a: [0,9]+[10,19] merge; [25,29] and [40,44] separate; b; c whole
  expect_equal(length(r), 5L)
  m <- r[[which(vapply(r, function(q) identical(q$rows, c(1L, 2L)), TRUE))]]
  expect_equal(c(m$start, m$end), c(0, 19))
  expect_equal(m$rel, c(0, 10))
  ## a gap of 5 bytes merges with merge_gap = 5
  r2 <- http_requests(url, off, len, merge_gap = 5)
  a <- Filter(function(q) q$url == "a", r2)
  expect_equal(length(a), 2L)       # [0,29] and [40,44] (gap 10 > 5)
  ## max_merge splits
  r3 <- http_requests(rep("a", 3), c(0, 10, 20), c(10, 10, 10), max_merge = 20)
  expect_equal(length(r3), 2L)
  expect_true(is.na(Filter(function(q) q$url == "c", r)[[1]]$start))
})

test_that("http_body() accepts 206 exact, cuts a 200 whole body, rejects short", {
  rq <- list(start = 2, end = 4)
  expect_equal(http_body(list(status_code = 206L, content = as.raw(1:3)), rq), as.raw(1:3))
  expect_equal(http_body(list(status_code = 200L, content = as.raw(0:9)), rq), as.raw(2:4))
  expect_null(http_body(list(status_code = 206L, content = as.raw(1:2)), rq))
  expect_null(http_body(list(status_code = 500L, content = raw()), list(start = NA)))
})

test_that("an HTTP Zarr store reads through curl: dataset, read, lazy array", {
  root <- file.path(tempdir(), "http-root")
  unlink(root, recursive = TRUE)
  dir.create(root)
  store_dir <- file.path(root, "s.zarr")
  dir.create(store_dir)
  writeLines('{"zarr_format": 2}', file.path(store_dir, ".zgroup"))
  x <- array(seq_len(7 * 5) / 4, c(7L, 5L))
  write_test_zarr(file.path(store_dir, "v"), x, chunk = c(3L, 2L),
                  dim_names = c("x", "y"), skip = list(c(2L, 2L)))
  consolidate(store_dir)
  srv <- local_range_server(root)
  on.exit(srv$stop(), add = TRUE)

  store <- zaro(paste0(srv$url, "/s.zarr"), verbose = FALSE)
  expect_true(S7::S7_inherits(store, zaro:::HttpStore))
  ds <- zaro_dataset(store, verbose = FALSE)
  expect_equal(ds$dims$v, c("x", "y"))
  p <- zaro_plan(ds, "v", verbose = FALSE)
  expect_true(all(p$transport == "http"))

  want <- x
  want[7, 5] <- -1                    # the skipped chunk (2, 2): fill value
  r <- zaro_read(store, "v", meta = ds$arrays$v, verbose = FALSE)
  expect_equal(as.vector(aperm(unname(r))), as.vector(want))
  skip_if_not_installed("altarr")
  a <- zaro_array(ds, "v", verbose = FALSE)
  expect_equal(a[seq_along(a)], as.vector(want))
  expect_equal(sum(a), sum(want))
})

test_that("virtual refs over HTTP: ranges merged, cap honoured, retries", {
  root <- file.path(tempdir(), "http-refs")
  unlink(root, recursive = TRUE)
  dir.create(root)
  ## 12 chunks of a 2 x 24 int32 array, chunks (2, 2), back to back in one
  ## file, then the same bytes in a "flaky" copy
  vals <- matrix(seq_len(48), 2)
  z <- function(v) memCompress(writeBin(as.integer(v), raw(), size = 4L), "gzip")
  chunks <- lapply(0:11, function(j) z(t(vals[, 2 * j + 1:2])))
  bytes <- do.call(c, chunks)
  writeBin(bytes, file.path(root, "src.bin"))
  writeBin(bytes, file.path(root, "flaky.bin"))
  off <- cumsum(c(0, lengths(chunks)))[1:12]
  srv <- local_range_server(root, delay = 0.2)
  on.exit(srv$stop(), add = TRUE)

  make_store <- function(file) {
    refs <- list(
      ".zgroup" = '{"zarr_format": 2}',
      "v/.zarray" = paste0('{"zarr_format": 2, "shape": [2, 24], "chunks": [2, 2], ',
                           '"dtype": "<i4", "compressor": {"id": "zlib", "level": 1}, ',
                           '"fill_value": 0, "filters": null, "order": "C"}'))
    for (j in 0:11) {
      refs[[sprintf("v/0.%d", j)]] <- list(paste0(srv$url, "/", file), off[j + 1],
                                          lengths(chunks)[j + 1])
    }
    json <- tempfile(fileext = ".json")
    writeLines(jsonlite::toJSON(list(version = 1L, refs = refs), auto_unbox = TRUE), json)
    zaro(paste0("reference+json://", json), verbose = FALSE)
  }

  ## contiguous ranges on one URL merge into one request
  store <- make_store("src.bin")
  p <- zaro_plan(store, "v", verbose = FALSE)
  expect_true(all(p$transport == "http"))
  got <- zaro_fetch(store, p, verbose = FALSE)
  expect_equal(attr(got, "stats")$requests, 1L)
  expect_equal(got, chunks, ignore_attr = "stats")
  r <- zaro_read(store, "v", verbose = FALSE)
  expect_equal(as.vector(r), as.vector(vals))

  ## without merging (every other chunk), concurrency stays within max_con
  p2 <- p[seq(1, 12, by = 2), ]
  before <- srv$stats()
  got2 <- zaro_fetch(store, p2, max_con = 2L, verbose = FALSE)
  expect_equal(got2, chunks[seq(1, 12, by = 2)], ignore_attr = "stats")
  expect_equal(attr(got2, "stats")$requests, 6L)
  expect_lte(srv$stats()$max_active, 2L)

  ## a 503 is retried
  flaky <- make_store("flaky.bin")
  got3 <- zaro_fetch(flaky, zaro_plan(flaky, "v", verbose = FALSE), verbose = FALSE)
  expect_equal(got3, chunks, ignore_attr = "stats")
  expect_equal(attr(got3, "stats")$requests, 2L)   # first try 503, retry ok
})

test_that("HTTP errors other than not-found are errors, not absent chunks", {
  root <- file.path(tempdir(), "http-err")
  unlink(root, recursive = TRUE)
  dir.create(root)
  writeBin(as.raw(1:10), file.path(root, "f.bin"))
  srv <- local_range_server(root)
  on.exit(srv$stop(), add = TRUE)
  ## a range past the end of the file comes back short: retried, then fails
  expect_error(http_fetch(paste0(srv$url, "/f.bin"), 5, 100, retries = 1L),
               "failed after 1 retries")
  ## a whole-object 404 is absent
  expect_null(http_fetch(paste0(srv$url, "/nope"), 0, NA)[[1]])
})
