## bytes for 64-bit integers, built from 32-bit halves of exact doubles
int64_bytes <- function(x, endian = "little") {
  hi <- floor(x / 2^32)                   # signed high word (negative x: < 0)
  lo <- x - hi * 2^32
  ## as a signed 32-bit word; R's NA_integer_ is the 0x80000000 pattern
  s32 <- function(u) {
    v <- ifelse(u >= 2^31, u - 2^32, u)
    out <- rep(NA_integer_, length(v))
    ok <- v != -2^31
    out[ok] <- as.integer(v[ok])
    out
  }
  w <- if (endian == "little") rbind(s32(lo), s32(hi)) else rbind(s32(hi), s32(lo))
  writeBin(as.vector(w), raw(), size = 4L, endian = endian)
}

test_that("int64 decodes exactly (both endians), not as reinterpreted doubles", {
  x <- c(0, 1, 708, -1, -708, 2^31, 2^32 + 5, -2^40, 2^53, -2^53)
  for (en in c("little", "big")) {
    expect_identical(read_typed(int64_bytes(x, en), "int64", length(x), en), x)
  }
})

test_that("uint64 and uint32 decode as unsigned", {
  x <- c(0, 1, 2^31, 2^32 - 1, 2^40, 2^53)
  for (en in c("little", "big")) {
    expect_identical(read_typed(int64_bytes(x, en), "uint64", length(x), en), x)
  }
  u <- c(0, 1, 2^31 - 1, 2^31, 2^32 - 1)
  s <- ifelse(u >= 2^31, u - 2^32, u)
  w <- rep(NA_integer_, length(s))           # NA_integer_ is 0x80000000
  w[s != -2^31] <- as.integer(s[s != -2^31])
  b <- writeBin(w, raw(), size = 4L)
  expect_identical(read_typed(b, "uint32", length(u), "little"), u)
})

test_that("float16 decodes from its bit fields", {
  bits <- c(0x0000, 0x3C00, 0xC000, 0x7BFF, 0x0001, 0x0400, 0x7C00, 0xFC00, 0x3555)
  want <- c(0, 1, -2, 65504, 2^-24, 2^-14, Inf, -Inf, 0.333251953125)
  for (en in c("little", "big")) {
    b <- writeBin(as.integer(bits), raw(), size = 2L, endian = en)
    expect_identical(read_typed(b, "float16", length(bits), en), want)
  }
  nan <- writeBin(0x7E00L, raw(), size = 2L)
  expect_true(is.nan(read_typed(nan, "float16", 1L, "little")))
})

test_that("an int64 V2 array reads through zaro_read()", {
  path <- file.path(tempdir(), "zarr-int64")
  unlink(path, recursive = TRUE)
  dir.create(path)
  x <- c(0, 708, 1416, -5, 2^40)
  writeLines('{"zarr_format": 2, "shape": [5], "chunks": [5], "dtype": "<i8", "compressor": null, "fill_value": null, "filters": null, "order": "C"}',
             file.path(path, ".zarray"))
  writeBin(int64_bytes(x), file.path(path, "0"))
  expect_identical(as.vector(zaro_read(zaro(path, verbose = FALSE), verbose = FALSE)), x)
})
