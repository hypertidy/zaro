## String data types (#22). The fixtures were written by zarr-python 3.2.1
## (numpy 2.5.1), V2 (zipped) and V3, zstd-compressed:
##   u     <U6  / fixed_length_utf32 (length_bytes 24)
##   s     |S5  / null_terminated_bytes (length_bytes 5)
##   v     |O + vlen-utf8 / string + vlen-utf8
##   grid  <U4 (3, 2), chunks (2, 2)
## Chunks are 2 elements, so every array has a short edge chunk.

u_ref <- c("alpha", "β-beta", "", "δέλτα", "e")
s_ref <- c("ab", "", "cdef", "g", "hijkl")
v_ref <- c("one", "", "three", "日本語", "a longer string here")
grid_ref <- matrix(c("a", "ccc", "e", "bb", "dddd", "ff"), 3, 2)

## the V2 store is zipped: its .zarray/.zgroup files would be hidden files
## in the package
v2_dir <- file.path(tempdir(), "zaro-strings-fixture")
if (!dir.exists(file.path(v2_dir, "strings-v2.zarr"))) {
  utils::unzip(test_path("fixtures", "strings-v2.zarr.zip"), exdir = v2_dir)
}
fixture <- function(fmt) {
  path <- if (fmt == 2) file.path(v2_dir, "strings-v2.zarr") else
    test_path("fixtures", "strings-v3.zarr")
  zaro(path, verbose = FALSE)
}

for (fmt in 2:3) {
  test_that(sprintf("V%d string arrays read as character", fmt), {
    st <- fixture(fmt)
    expect_identical(as.vector(zaro_read(st, "u", verbose = FALSE)), u_ref)
    expect_identical(as.vector(zaro_read(st, "s", verbose = FALSE)), s_ref)
    expect_identical(as.vector(zaro_read(st, "v", verbose = FALSE)), v_ref)
    g <- zaro_read(st, "grid", verbose = FALSE)
    expect_identical(matrix(g, nrow(g)), grid_ref)
    expect_identical(dim(g)[[1]], 3L)
  })
}

test_that("string dtypes are named, and fill values are character", {
  m2 <- zaro_meta(fixture(2), "u", verbose = FALSE)
  m3 <- zaro_meta(fixture(3), "s", verbose = FALSE)
  expect_identical(m2@data_type, "fixed_length_utf32")
  expect_identical(m3@data_type, "null_terminated_bytes")
  expect_identical(zaro_meta(fixture(3), "v", verbose = FALSE)@data_type, "string")
  expect_identical(string_spec(m2)$length_bytes, 24L)
  expect_identical(string_spec(m3)$length_bytes, 5L)
  expect_identical(m2@fill_value, "")
  expect_identical(coerce_fill_value(NULL, "string"), NA_character_)
  expect_identical(coerce_fill_value(0, "fixed_length_utf32"), NA_character_)
  expect_identical(normalise_string_dtype("fixed_length_ascii"),
                   "null_terminated_bytes")
  expect_identical(normalise_string_dtype("variable_length_utf8"), "string")
})

test_that("a selection of strings, and several string arrays in one fetch", {
  st <- fixture(3)
  expect_identical(as.vector(zaro_read(st, "u", start = 1, count = 3,
                                       verbose = FALSE)), u_ref[2:4])
  xs <- zaro_read_arrays(st, c("u", "v"), verbose = FALSE)
  expect_identical(as.vector(xs$u), u_ref)
  expect_identical(as.vector(xs$v), v_ref)
})

test_that("big-endian UTF-32 (V2 >U) decodes", {
  meta <- parse_zarray(charToRaw(paste0(
    '{"zarr_format": 2, "shape": [2], "chunks": [2], "dtype": ">U3", ',
    '"compressor": null, "filters": null, "fill_value": "", "order": "C"}')))
  cp <- c(utf8ToInt("ab"), 0L, utf8ToInt("éxy"))
  buf <- writeBin(cp, raw(), size = 4L, endian = "big")
  expect_identical(decode_chunk(buf, meta), c("ab", "éxy"))
})

test_that("vlen strings without the vlen-utf8 codec are refused", {
  meta <- parse_zarray(charToRaw(paste0(
    '{"zarr_format": 2, "shape": [1], "chunks": [1], "dtype": "|O", ',
    '"compressor": null, "filters": [{"id": "json2"}], "fill_value": null}')))
  expect_error(suppressWarnings(decode_chunk(raw(8), meta)), "vlen-utf8")
})

test_that("zaro_array() refuses strings and points to zaro_read()", {
  skip_if_not_installed("altarr")
  expect_error(zaro_array(fixture(3), "u", verbose = FALSE), "zaro_read")
})
