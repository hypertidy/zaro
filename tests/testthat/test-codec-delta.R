## encode like a TIFF writer (predictor 2): per row along the last axis,
## differences in the sample type with wraparound
hdelta_encode <- function(x, bits, signed) {
  d <- dim(x)
  ncol <- d[length(d)]
  m <- matrix(aperm(x, rev(seq_along(d))), ncol = ncol, byrow = TRUE)
  out <- m
  if (ncol > 1L) out[, -1] <- m[, -1] - m[, -ncol]
  out <- if (signed) (out + 2^(bits - 1)) %% 2^bits - 2^(bits - 1) else out %% 2^bits
  v <- as.vector(t(out))
  if (!signed) v <- ifelse(v >= 2^(bits - 1), v - 2^bits, v)
  writeBin(as.integer(v), raw(), size = bits %/% 8L, endian = "little")
}

## a one-chunk V2 array with filters [imagecodecs_delta] and zlib, given in
## Zarr (C) order as an R array of the Zarr shape
write_delta_zarr <- function(path, x, np_dtype, bits, signed) {
  unlink(path, recursive = TRUE)
  dir.create(path)
  shp <- paste0("[", paste(dim(x), collapse = ", "), "]")
  writeLines(sprintf(paste0(
    '{"zarr_format": 2, "shape": %s, "chunks": %s, "dtype": "%s", ',
    '"compressor": {"id": "zlib", "level": 1}, "fill_value": 0, "order": "C", ',
    '"filters": [{"id": "imagecodecs_delta", "axis": -1, "dist": 1, ',
    '"dtype": "%s", "shape": %s}]}'), shp, shp, np_dtype, np_dtype, shp),
    file.path(path, ".zarray"))
  key <- paste(rep("0", length(dim(x))), collapse = ".")
  writeBin(memCompress(hdelta_encode(x, bits, signed), "gzip"), file.path(path, key))
  zaro(path, verbose = FALSE)
}

test_that("int16 horizontal delta decodes, including wraparound", {
  set.seed(1)
  x <- matrix(sample(-32768:32767, 4 * 6, replace = TRUE), 4, 6)
  x[1, ] <- c(-32000, 32000, -32000, 32767, -32768, 0)   # differences overflow int16
  s <- write_delta_zarr(file.path(tempdir(), "delta-i2"), x, "<i2", 16L, TRUE)
  expect_equal(unname(unclass(zaro_read(s, verbose = FALSE))), x, ignore_attr = TRUE)
})

test_that("uint16 and int8, and a 3D tile shape", {
  set.seed(2)
  u <- matrix(sample(0:65535, 3 * 5, replace = TRUE), 3, 5)
  su <- write_delta_zarr(file.path(tempdir(), "delta-u2"), u, "<u2", 16L, FALSE)
  expect_equal(as.vector(zaro_read(su, verbose = FALSE)), as.vector(u))

  b <- array(sample(-128:127, 2 * 3 * 4, replace = TRUE), c(2, 3, 4))
  sb <- write_delta_zarr(file.path(tempdir(), "delta-i1"), b, "|i1", 8L, TRUE)
  expect_equal(as.vector(zaro_read(sb, verbose = FALSE)), as.vector(b))
})

test_that("float data with a horizontal delta is refused clearly", {
  meta <- parse_zarray(charToRaw(paste0(
    '{"zarr_format": 2, "shape": [2, 2], "chunks": [2, 2], "dtype": "<f4", ',
    '"compressor": null, "fill_value": 0, "order": "C", ',
    '"filters": [{"id": "imagecodecs_delta", "axis": -1, "dist": 1}]}')))
  expect_error(decode_chunk(as.raw(rep(0, 16)), meta), "TIFF predictor 3")
})

test_that("zstd, gzip and bz2 decode with base R, no arrow", {
  raw_in <- writeBin(rep(1234L, 512 * 512), raw(), size = 2L)
  for (type in c("zstd", "gzip", "bz2")) {
    base_type <- switch(type, bz2 = "bzip2", type)
    packed <- tryCatch(memCompress(raw_in, base_type), error = function(e) NULL)
    if (is.null(packed)) next              # R built without libzstd
    expect_identical(decode_one(packed, list(name = type), NULL), raw_in,
                     label = type)
  }
  expect_error(base_decompress(as.raw(1:10), "gzip", list()))
})

test_that("chunks compressing better than 20:1 decompress in full", {
  skip_if_not_installed("arrow")
  ## a constant 512 x 512 int16 chunk: zstd/gzip compress it far beyond 20:1;
  ## arrow writes streaming frames, which base R also reads
  for (type in c("zstd", "gzip")) {
    raw_in <- writeBin(rep(1234L, 512 * 512), raw(), size = 2L)
    codec <- arrow::Codec$create(type)
    sink <- arrow::BufferOutputStream$create()
    stream <- arrow::CompressedOutputStream$create(sink, codec)
    stream$write(arrow::buffer(raw_in))
    stream$close()
    packed <- as.raw(sink$finish())
    expect_gt(length(raw_in) / length(packed), 20)
    expect_identical(arrow_decompress(packed, type, list()), raw_in, label = type)
    expect_identical(base_decompress(packed, type, list()), raw_in, label = type)
  }
})

test_that("a chunk that fails to decode names its key and byte range", {
  meta <- parse_zarray(charToRaw(paste0(
    '{"zarr_format": 2, "shape": [4], "chunks": [4], "dtype": "<i2", ',
    '"compressor": {"id": "zstd", "level": 1}, "fill_value": 0, "order": "C", "filters": null}')))
  ref <- data.frame(key = "v/0", path = "https://example.com/f.tif", offset = 100, size = 8)
  expect_error(decode_planned(as.raw(1:8), meta, ref),
               "decoding chunk 'v/0' from https://example.com/f.tif \\[offset 100, 8 bytes\\].*changed")
})
