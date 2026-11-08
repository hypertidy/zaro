## Regression for #1: BRAN2023 `temp` through VirtualiZarr is int16 with
## filters [shuffle] and compressor zlib (Zarr V2), and failed with "unknown
## codec 'zlib'/'shuffle'" and a chunk length mismatch. This builds the same
## codec chain locally: edge chunks padded, one chunk never written.

## numcodecs Shuffle: byte k of every element, then byte k + 1, ...
shuffle_bytes <- function(b, elementsize) {
  as.vector(t(matrix(b, nrow = elementsize)))
}

## a V2 int16 array of Zarr shape dim(x), C order, filters [shuffle] + zlib;
## chunks listed in `skip` (0-based, Zarr order) are not written
write_shuffle_zarr <- function(path, x, chunk, fill = -32768L, skip = list()) {
  unlink(path, recursive = TRUE)
  dir.create(path)
  jarr <- function(v) paste0("[", paste(v, collapse = ", "), "]")
  writeLines(sprintf(paste0(
    '{"zarr_format": 2, "shape": %s, "chunks": %s, "dtype": "<i2", ',
    '"compressor": {"id": "zlib", "level": 4}, "fill_value": %d, ',
    '"filters": [{"id": "shuffle", "elementsize": 2}], "order": "C"}'),
    jarr(dim(x)), jarr(chunk), fill), file.path(path, ".zarray"))
  d <- dim(x)
  for (i in seq_len(ceiling(d[1] / chunk[1])) - 1L) {
    for (j in seq_len(ceiling(d[2] / chunk[2])) - 1L) {
      if (any(vapply(skip, function(s) all(s == c(i, j)), logical(1)))) next
      rows <- seq.int(i * chunk[1] + 1L, min(d[1], (i + 1L) * chunk[1]))
      cols <- seq.int(j * chunk[2] + 1L, min(d[2], (j + 1L) * chunk[2]))
      padded <- matrix(fill, chunk[1], chunk[2])
      padded[seq_along(rows), seq_along(cols)] <- x[rows, cols]
      b <- writeBin(as.integer(t(padded)), raw(), size = 2L, endian = "little")
      writeBin(memCompress(shuffle_bytes(b, 2L), "gzip"),
               file.path(path, paste(i, j, sep = ".")))
    }
  }
  zaro(path, verbose = FALSE)
}

test_that("int16 with shuffle + zlib reads, edge and absent chunks too", {
  set.seed(1)
  x <- matrix(sample(-32767:32767, 5 * 7, replace = TRUE), 5, 7)
  s <- write_shuffle_zarr(file.path(tempdir(), "shuffle-i2"), x, c(2L, 4L),
                          skip = list(c(2L, 1L)))
  expected <- x
  expected[5, 5:7] <- -32768L          # the unwritten chunk reads as fill
  v <- zaro_read(s, verbose = FALSE)
  expect_equal(as.vector(v), as.vector(expected))
  expect_equal(dim(v), dim(x), ignore_attr = TRUE)

  ## a hyperslab across chunk boundaries
  w <- zaro_read(s, start = c(1, 2), count = c(3, 4), verbose = FALSE)
  expect_equal(as.vector(w), as.vector(expected[2:4, 3:6]))
})
