## Parquet reference manifests read with nanoparquet: VirtualiZarr shards
## and single-file kerchunk-style tables, local and over HTTP

## array a: (y = 4, x = 6) doubles, chunks (2, 3), C order -> 4 chunks.
## Chunk bytes go into one source file; one chunk is inline instead.
make_vz <- function(dir) {
  unlink(dir, recursive = TRUE)
  dir.create(file.path(dir, "a"), recursive = TRUE)
  x <- matrix(as.double(1:24), 4, 6, byrow = TRUE)   # x[y, x]
  chunk_bytes <- function(cy, cx) {
    v <- x[cy * 2 + 1:2, cx * 3 + 1:3]
    writeBin(as.vector(t(v)), raw(), size = 8L, endian = "little")  # C order
  }
  grid <- expand.grid(cx = 0:1, cy = 0:1)[, c("cy", "cx")]   # C-order linear
  src <- file.path(dir, "source.bin")
  bytes <- lapply(seq_len(nrow(grid)), function(i) chunk_bytes(grid$cy[i], grid$cx[i]))
  writeBin(do.call(c, bytes), src)
  offs <- c(0, cumsum(lengths(bytes)))[seq_along(bytes)]
  path <- rep(normalizePath(src), 4)
  raw_col <- vector("list", 4)
  path[3] <- NA                      # chunk (1, 0) is inline
  raw_col[[3]] <- bytes[[3]]
  rows <- data.frame(path = path, offset = offs, size = lengths(bytes))
  ## shard 0 by nanoparquet, shard 1 by arrow when installed (as pyarrow
  ## writes them: int64 offset, binary raw)
  s0 <- rows[1:2, ]; s0$raw <- raw_col[1:2]
  nanoparquet::write_parquet(s0, file.path(dir, "a", "refs.0.parq"))
  if (requireNamespace("arrow", quietly = TRUE)) {
    s1 <- arrow::arrow_table(path = rows$path[3:4],
                             offset = arrow::Array$create(rows$offset[3:4], arrow::int64()),
                             size = arrow::Array$create(rows$size[3:4], arrow::int64()),
                             raw = arrow::Array$create(raw_col[3:4], arrow::binary()))
    arrow::write_parquet(s1, file.path(dir, "a", "refs.1.parq"))
  } else {
    s1 <- rows[3:4, ]; s1$raw <- raw_col[3:4]
    nanoparquet::write_parquet(s1, file.path(dir, "a", "refs.1.parq"))
  }
  writeLines(paste0(
    '{"zarr_consolidated_format": 1, "metadata": {',
    '".zgroup": {"zarr_format": 2}, ".zattrs": {}, ',
    '"a/.zarray": {"zarr_format": 2, "shape": [4, 6], "chunks": [2, 3], ',
    '"dtype": "<f8", "compressor": null, "filters": null, ',
    '"fill_value": null, "order": "C"}, ',
    '"a/.zattrs": {"_ARRAY_DIMENSIONS": ["y", "x"]}}}'),
    file.path(dir, ".zmetadata"))
  list(dir = dir, x = x)
}

test_that("read_refs_parquet: blob columns become lists of raw, int64 double", {
  d <- make_vz(file.path(tempdir(), "pq-refs-unit.parq"))
  t1 <- read_refs_parquet(file.path(d$dir, "a", "refs.1.parq"))
  expect_s3_class(t1, "data.frame")
  expect_type(t1$raw, "list")
  expect_false(inherits(t1$raw, "blob"))
  expect_type(t1$raw[[1]], "raw")
  expect_null(t1$raw[[2]])
  expect_type(t1$offset, "double")
})

test_that("VirtualiZarr shards (nanoparquet- and arrow-written) read", {
  d <- make_vz(file.path(tempdir(), "pq-refs-vz.parq"))
  st <- zaro(paste0("virtualizarr://", d$dir), verbose = FALSE)
  v <- zaro_read(st, "a", verbose = FALSE)
  expect_equal(unname(v), d$x, ignore_attr = TRUE)
  p <- zaro_plan(st, "a", verbose = FALSE)
  expect_equal(sort(table(p$transport)[c("inline", "local")]),
               sort(c(inline = 1L, local = 3L)), ignore_attr = TRUE)
})

## kerchunk-style single table: key, url, offset, length, raw; metadata
## rows inline (text bytes), data rows by reference
make_kc <- function(file, d) {
  zarray <- '{"zarr_format": 2, "shape": [4, 6], "chunks": [2, 3], "dtype": "<f8", "compressor": null, "filters": null, "fill_value": null, "order": "C"}'
  src <- normalizePath(file.path(d$dir, "source.bin"))
  tbl <- data.frame(
    key = c(".zgroup", "a/.zarray", "a/.zattrs", "a/0.0", "a/0.1", "a/1.0", "a/1.1"),
    url = c(NA, NA, NA, rep(src, 4)),
    offset = c(NA, NA, NA, 0, 48, 96, 144),
    length = c(NA, NA, NA, 48, 48, 48, 48))
  tbl$raw <- list(charToRaw('{"zarr_format": 2}'), charToRaw(zarray),
                  charToRaw('{"_ARRAY_DIMENSIONS": ["y", "x"]}'),
                  NULL, NULL, NULL, NULL)
  nanoparquet::write_parquet(tbl, file)
  file
}

test_that("kerchunk Parquet: per-row refs and inline raw metadata", {
  d <- make_vz(file.path(tempdir(), "pq-refs-kc-src.parq"))
  f <- make_kc(file.path(tempdir(), "pq-refs-kc.parquet"), d)
  st <- zaro(paste0("reference+parquet://", f), verbose = FALSE)
  expect_type(st@refs[["a/.zarray"]], "raw")
  expect_length(st@refs[["a/0.1"]], 3L)
  expect_equal(unname(zaro_read(st, "a", verbose = FALSE)), d$x,
               ignore_attr = TRUE)
})

test_that("kerchunk Parquet over HTTP is downloaded, then read", {
  d <- make_vz(file.path(tempdir(), "pq-refs-kc-http-src.parq"))
  srvdir <- file.path(tempdir(), "pq-refs-srv")
  dir.create(srvdir, showWarnings = FALSE)
  make_kc(file.path(srvdir, "refs.parquet"), d)
  srv <- local_range_server(srvdir)
  on.exit(srv$stop(), add = TRUE)
  st <- zaro(paste0("reference+parquet://", srv$url, "/refs.parquet"),
             verbose = FALSE)
  expect_equal(unname(zaro_read(st, "a", verbose = FALSE)), d$x,
               ignore_attr = TRUE)
})
