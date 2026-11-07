## expected values: x with the empty inner chunks and missing shards filled
fill_regions <- function(x, inner, shard, empty = list(), missing = list(), fill = -1) {
  d <- dim(x)
  set <- function(x, st, n) {
    st <- st + 1L
    ext <- pmin(n, d - st + 1L)
    if (any(ext <= 0L)) return(x)
    do.call(`[<-`, c(list(x), Map(function(a, k) seq.int(a, length.out = k), st, ext),
                     list(value = fill)))
  }
  for (e in empty) x <- set(x, e * inner, inner)
  for (m in missing) x <- set(x, m * shard, shard)
  x
}

test_that("sharded V3 (index at end, crc32c): read, lazy array, plan", {
  x <- array(seq_len(9 * 7) / 8, c(9L, 7L))
  path <- file.path(tempdir(), "shard-end")
  unlink(path, recursive = TRUE)
  empty <- list(c(1L, 0L))
  missing <- list(c(1L, 1L))
  write_sharded_zarr(path, x, shard = c(6L, 4L), inner = c(3L, 2L),
                     empty = empty, missing = missing, dim_names = c("x", "y"))
  want <- fill_regions(x, c(3L, 2L), c(6L, 4L), empty, missing)
  store <- zaro(path, verbose = FALSE)
  meta <- zaro_meta(store, verbose = FALSE)
  expect_equal(meta@chunk_shape, c(2L, 3L))           # inner, Zarr order
  expect_equal(attr(meta, "shard")$shape, c(4L, 6L))
  expect_output(print(meta), "shards: \\[4 x 6\\] \\(index at end\\)")

  r <- zaro_read(store, verbose = FALSE)
  expect_equal(as.vector(aperm(unname(r))), as.vector(want))

  skip_if_not_installed("altarr")
  a <- zaro_array(store, verbose = FALSE)
  expect_equal(dim(a), dim(x))
  expect_equal(a[seq_along(a)], as.vector(want))
  expect_equal(unname(altarr::altarr_extract(a, 4:8, 2:6)), want[4:8, 2:6])

  p <- zaro_plan(store, verbose = FALSE)
  expect_equal(nrow(p), 3L * 4L)                      # inner grid 3 x 4
  ## present inner chunks are byte ranges of their shard file
  present <- p$transport == "local"
  expect_true(all(!is.na(p$length[present])))
  expect_true(all(p$length[present] == 3 * 2 * 8))
  expect_true(all(grepl("/c/[0-9]+/[0-9]+$", p$url[present])))
  ## absent: the empty inner chunk (1, 0), and the inner chunks of the
  ## missing shard (1, 1) that exist in the 3 x 4 inner grid: (2, 2), (2, 3)
  expect_equal(sum(p$transport == "absent"), 3L)
})

test_that("sharded V3 (index at start, no crc), 3D, edge padding", {
  x <- array(seq_len(5 * 4 * 3), c(5L, 4L, 3L)) + 0.5
  path <- file.path(tempdir(), "shard-start")
  unlink(path, recursive = TRUE)
  write_sharded_zarr(path, x, shard = c(4L, 4L, 2L), inner = c(2L, 2L, 1L),
                     index_location = "start", crc = FALSE)
  store <- zaro(path, verbose = FALSE)
  r <- zaro_read(store, verbose = FALSE)
  expect_equal(as.vector(aperm(unname(r))), as.vector(x))
  ## a hyperslab touching a single inner chunk plans one byte range
  p <- zaro_plan(store, start = c(1, 2, 0), count = c(1, 1, 2), verbose = FALSE)
  expect_equal(nrow(p), 1L)
  expect_equal(p$length, 2 * 2 * 1 * 8)
  ## zaro_chunk() (one inner chunk) goes through the same plan
  ch <- zaro_chunk(store, "", c(0L, 0L, 0L), verbose = FALSE)
  expect_equal(as.vector(ch$values), as.vector(aperm(x[1:2, 1:2, 1, drop = FALSE])))
})

test_that("sharded V3 over HTTP: suffix-range index reads, pooled fetch", {
  skip_if_not_installed("curl")
  root <- file.path(tempdir(), "shard-http")
  unlink(root, recursive = TRUE)
  dir.create(root)
  x <- array(seq_len(8 * 8) / 2, c(8L, 8L))
  write_sharded_zarr(file.path(root, "a.zarr"), x, shard = c(4L, 4L),
                     inner = c(2L, 2L), missing = list(c(1L, 1L)))
  want <- fill_regions(x, c(2L, 2L), c(4L, 4L), missing = list(c(1L, 1L)))
  srv <- local_range_server(root)
  on.exit(srv$stop(), add = TRUE)
  store <- zaro(paste0(srv$url, "/a.zarr"), verbose = FALSE)
  before <- srv$stats()$requests
  p <- zaro_plan(store, verbose = FALSE)
  ## 1 metadata GET + 4 shard index reads (the missing shard is a 404)
  expect_lte(srv$stats()$requests - before, 5L)
  expect_true(all(p$transport[!is.na(p$url)] == "http"))
  expect_equal(sum(p$transport == "absent"), 4L)
  r <- zaro_read(store, verbose = FALSE)
  expect_equal(as.vector(aperm(unname(r))), as.vector(want))
})

test_that("V3 chunk keys honour the encoding and separator", {
  m <- function(enc, sep) {
    j <- sprintf(paste0('{"zarr_format": 3, "node_type": "array", "shape": [4, 4], ',
      '"data_type": "float64", "chunk_grid": {"name": "regular", "configuration": ',
      '{"chunk_shape": [2, 2]}}, "chunk_key_encoding": {"name": "%s"%s}, ',
      '"codecs": [{"name": "bytes"}], "fill_value": 0}'), enc,
      if (is.na(sep)) "" else sprintf(', "configuration": {"separator": "%s"}', sep))
    parse_zarr_json(charToRaw(j))
  }
  expect_equal(chunk_key(m("default", NA), c(1L, 0L)), "c/1/0")
  expect_equal(chunk_key(m("default", "."), c(1L, 0L)), "c.1.0")
  expect_equal(chunk_key(m("v2", NA), c(1L, 0L)), "1.0")
  expect_equal(chunk_key(m("v2", "/"), c(1L, 0L)), "1/0")
})

test_that("a crc32c codec on chunks is stripped", {
  meta <- parse_zarr_json(charToRaw(paste0(
    '{"zarr_format": 3, "node_type": "array", "shape": [2], "data_type": "float64", ',
    '"chunk_grid": {"name": "regular", "configuration": {"chunk_shape": [2]}}, ',
    '"chunk_key_encoding": {"name": "default"}, ',
    '"codecs": [{"name": "bytes", "configuration": {"endian": "little"}}, {"name": "crc32c"}], ',
    '"fill_value": 0}')))
  b <- c(writeBin(c(1.5, -2), raw(), size = 8L, endian = "little"), as.raw(1:4))
  expect_equal(decode_chunk(b, meta), c(1.5, -2))
})

test_that("sharded arrays written by zarr-python read exactly", {
  py <- Sys.which("python3")
  skip_if(!nzchar(py), "python3 not available")
  has_zarr3 <- suppressWarnings(system2(py, c("-I", "-c", shQuote(
    "import zarr, sys; sys.exit(0 if int(zarr.__version__.split('.')[0]) >= 3 else 1)")),
    stdout = FALSE, stderr = FALSE)) == 0L
  skip_if(!has_zarr3, "zarr-python >= 3 not available")
  out <- file.path(tempdir(), "zarr-python-sharded")
  unlink(out, recursive = TRUE)
  status <- system2(py, c("-I", shQuote(test_path("zarr_python_sharded.py")), shQuote(out)),
                    stdout = FALSE, stderr = FALSE)
  skip_if(status != 0L, "zarr-python fixture script failed")
  cases <- list(f32_end_zstd = list("double", 4L, TRUE),
                u16_start_gzip = list("integer", 2L, FALSE),
                i32_partial = list("integer", 4L, TRUE))
  for (nm in names(cases)) {
    k <- cases[[nm]]
    store <- zaro(file.path(out, paste0(nm, ".zarr")), verbose = FALSE)
    meta <- zaro_meta(store, verbose = FALSE)
    expect_false(is.null(attr(meta, "shard")))
    want <- readBin(file.path(out, paste0(nm, ".bin")), k[[1]], n = prod(meta@shape),
                    size = k[[2]], signed = k[[3]], endian = "little")
    got <- zaro_read(store, verbose = FALSE)
    expect_equal(as.vector(aperm(unname(got))), as.double(want), tolerance = 0,
                 label = nm)
  }
  ## the partially written array has missing shards and empty inner chunks
  p <- zaro_plan(zaro(file.path(out, "i32_partial.zarr"), verbose = FALSE),
                 verbose = FALSE)
  expect_equal(sum(p$transport == "absent"), 52L)
})
