## a byterefs SQLite store over a local source file; the source is recorded
## as /old/<file> and the 'local' remap target points at the real file
make_byterefs <- function(format = "byterefs/0.1", default_target = NULL,
                          codec = '{"compressor":{"id":"zlib","level":1},"filters":null}') {

  ## source file: 7 junk bytes, then two zlib chunks of a 2 x 4 int32 array
  ## chunked (2, 2); C order chunk (0,0) = 1,2,5,6 and (0,1) = 3,4,7,8
  z <- function(v) memCompress(writeBin(as.integer(v), raw(), size = 4L,
                                        endian = "little"), "gzip")
  c0 <- z(c(1, 2, 5, 6)); c1 <- z(c(3, 4, 7, 8))
  src <- tempfile(fileext = ".bin")
  writeBin(c(as.raw(1:7), c0, c1), src)

  db <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbExecute(con, "CREATE TABLE meta(key TEXT PRIMARY KEY, value TEXT)")
  DBI::dbExecute(con, "INSERT INTO meta VALUES('format_version', ?)", params = list(format))
  if (!is.null(default_target)) {
    DBI::dbExecute(con, "INSERT INTO meta VALUES('default_target', ?)",
                   params = list(default_target))
  }
  DBI::dbExecute(con, "CREATE TABLE sources(source_id INTEGER PRIMARY KEY, source TEXT)")
  DBI::dbExecute(con, "INSERT INTO sources VALUES(1, ?)", params = list(paste0("/old/", basename(src))))
  DBI::dbExecute(con, "CREATE TABLE remap(target TEXT, prefix TEXT, replacement TEXT)")
  DBI::dbExecute(con, "INSERT INTO remap VALUES('local', '/old/', ?)",
                 params = list(paste0(dirname(src), "/")))
  DBI::dbExecute(con, 'CREATE TABLE arrays(array TEXT PRIMARY KEY, kind TEXT, storage TEXT,
    dims TEXT, shape TEXT, chunks TEXT, dtype TEXT, fill TEXT, codec TEXT, attrs TEXT,
    affine TEXT, "values" BLOB)')
  DBI::dbExecute(con, "INSERT INTO arrays VALUES('v', 'data', 'referenced', '[\"y\",\"x\"]',
    '[2,6]', '[2,2]', '<i4', '-1', ?, '{\"units\":\"m\"}', NULL, NULL)", params = list(codec))
  DBI::dbExecute(con, "CREATE TABLE placements(array TEXT, source_id INTEGER, source_array TEXT,
    src_offset TEXT, count TEXT, dest TEXT)")
  DBI::dbExecute(con, "INSERT INTO placements VALUES('v', 1, 'v', '[0,0]', '[2,6]', '[0,0]')")
  DBI::dbExecute(con, "INSERT INTO arrays VALUES('x', 'coord', 'affine', '[\"x\"]',
    '[6]', '[6]', '<f8', NULL, NULL, NULL, '{\"start\":10,\"step\":0.5}', NULL)")
  DBI::dbExecute(con, 'CREATE TABLE v(source_id INTEGER, dim_0 INTEGER, dim_1 INTEGER,
    present INTEGER, "offset" BIGINT, size BIGINT, info TEXT, data BLOB)')
  DBI::dbExecute(con, 'INSERT INTO v VALUES(1, 0, 0, 1, 7, ?, NULL, NULL), (1, 0, 1, 1, ?, ?, NULL, NULL),
    (1, 0, 2, 0, NULL, NULL, NULL, NULL)',
    params = list(length(c0), 7 + length(c0), length(c1)))
  DBI::dbExecute(con, 'CREATE VIEW refs_v AS SELECT dim_0, dim_1, present,
    CASE WHEN "offset" IS NULL THEN NULL ELSE s.source END AS path, "offset", size, info, data
    FROM v JOIN sources s USING(source_id)')
  DBI::dbDisconnect(con)
  db
}

test_that("mdimrefs:// reads chunks through the refs_ view", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  db <- make_byterefs()
  st <- zaro(paste0("mdimrefs://", db), target = "local", verbose = FALSE)
  expect_true(".zmetadata" %in% zaro_list(st))
  v <- zaro_read(st, "v", verbose = FALSE)
  expect_equal(as.vector(t(matrix(v, 2))), c(1:4, -1L, -1L, 5:8, -1L, -1L))
  expect_equal(as.vector(zaro_read(st, "x", verbose = FALSE)), 10 + 0.5 * 0:5)
})

test_that("byterefs/0.1 and legacy mdim-refs/0.2 identifiers are accepted", {
  expect_true(mdimrefs_format_ok("byterefs/0.1"))
  expect_true(mdimrefs_format_ok("mdim-refs/0.2"))
  ## only the 0.1 identifiers (decision 0003), not any version under the names
  expect_false(mdimrefs_format_ok("byterefs/0.2"))
  expect_false(mdimrefs_format_ok("mdim-refs/0.3"))
  expect_false(mdimrefs_format_ok("kerchunk/1"))
  expect_false(mdimrefs_format_ok(character()))
  expect_false(mdimrefs_format_ok(NA_character_))
})


test_that("byterefs:// scheme; other versions warn or fail clearly", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  st <- zaro(paste0("byterefs://", make_byterefs()), target = "local", verbose = FALSE)
  expect_equal(as.vector(zaro_read(st, "x", verbose = FALSE)), 10 + 0.5 * 0:5)
  expect_warning(zaro(paste0("byterefs://", make_byterefs("byterefs/0.2")),
                      target = "local", verbose = FALSE),
                 "read as byterefs/0.1")
  expect_error(zaro(paste0("byterefs://", make_byterefs("mdim-refs/0.1")), verbose = FALSE),
               "not a byterefs store.*byterefs/0.1 and mdim-refs/0.2")
})

test_that("meta.default_target is applied when no target is given (decision 0004)", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  db <- make_byterefs(default_target = "local")
  st <- zaro(paste0("byterefs://", db), verbose = FALSE)
  expect_equal(st@target, "local")
  v <- zaro_read(st, "v", verbose = FALSE)
  expect_equal(as.vector(t(matrix(v, 2))), c(1:4, -1L, -1L, 5:8, -1L, -1L))
  p <- zaro_plan(st, "v", verbose = FALSE)
  expect_false(any(startsWith(stats::na.omit(p$path), "/old/")))

  ## target = "" forces the verbatim sources (here unreadable: /old/...)
  verb <- zaro(paste0("byterefs://", db), target = "", verbose = FALSE)
  expect_equal(verb@target, "")
  pv <- zaro_plan(verb, "v", verbose = FALSE)
  expect_true(all(startsWith(stats::na.omit(pv$path), "/old/")))

  ## no default target: verbatim, as before
  expect_equal(zaro(paste0("byterefs://", make_byterefs()), verbose = FALSE)@target, "")

  ## a target with no rules warns
  expect_warning(zaro(paste0("byterefs://", db), target = "nope", verbose = FALSE),
                 "no remap rules for target 'nope'")
})


## decision 0007: arrays.codec as a Zarr V3 codec chain, or the V2 form
V3_ZLIB <- '[{"name":"bytes","configuration":{"endian":"little"}},
             {"name":"numcodecs.zlib","configuration":{"level":1}}]'

test_that("a V3 codec chain in arrays.codec reads as the V2 form does", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  st <- zaro(paste0("byterefs://", make_byterefs(codec = V3_ZLIB)), target = "local",
             verbose = FALSE)
  v <- zaro_read(st, "v", verbose = FALSE)
  expect_equal(as.vector(t(matrix(v, 2))), c(1:4, -1L, -1L, 5:8, -1L, -1L))
})

test_that("V3 chains map to V2 compressor and filters in decode order", {
  ch <- function(...) jsonlite::toJSON(list(...), auto_unbox = TRUE)
  bytes <- list(name = "bytes", configuration = list(endian = "little"))
  delta <- list(name = "HorizontalDeltaCodec", configuration = list(axis = -1, dist = 1))
  x <- mdimrefs_codec(ch(delta, bytes, list(name = "numcodecs.shuffle"),
                         list(name = "zstd", configuration = list(level = 3))), "<u2")
  expect_identical(x$compressor, list(id = "zstd", level = 3L))
  expect_identical(vapply(x$filters, `[[`, "", "id"), c("imagecodecs_delta", "shuffle"))
  expect_identical(x$filters[[2]]$elementsize, 2L)       # item size of <u2
  ## no bytes-to-bytes codec: no compressor
  x <- mdimrefs_codec(ch(bytes), "<i2")
  expect_null(x$compressor); expect_null(x$filters)
  ## a trailing crc32c is the last codec applied, so the first undone
  x <- mdimrefs_codec(ch(bytes, list(name = "gzip"), list(name = "crc32c")), "<f4")
  expect_identical(x$compressor$id, "crc32c")
  expect_identical(x$filters[[1]]$id, "gzip")
  ## a one-byte dtype may leave endian out
  expect_silent(mdimrefs_codec(ch(list(name = "bytes")), "|u1"))
  ## the V2 form is an object
  x <- mdimrefs_codec('{"compressor":{"id":"zlib","level":1},"filters":null}', "<i2")
  expect_identical(x$compressor$id, "zlib")
})

test_that("chains zaro cannot read are refused clearly", {
  ch <- function(...) jsonlite::toJSON(list(...), auto_unbox = TRUE)
  bytes <- list(name = "bytes", configuration = list(endian = "little"))
  expect_error(mdimrefs_codec(ch(bytes, list(name = "lzma")), "<i2"),
               "unknown codec 'lzma'")
  expect_error(mdimrefs_codec(ch(list(name = "sharding_indexed")), "<i2"),
               "sharding_indexed is not allowed in arrays.codec")
  expect_error(mdimrefs_codec(ch(list(name = "numcodecs.zlib"), bytes), "<i2"),
               "then one array-to-bytes codec")
  expect_error(mdimrefs_codec(ch(list(name = "gzip")), "<i2"),
               "then one array-to-bytes codec")
  expect_error(mdimrefs_codec(ch(list(name = "bytes", configuration = list(endian = "big"))), "<i2"),
               "endian 'big' disagrees with dtype '<i2'")
  expect_error(mdimrefs_codec(ch(list(name = "bytes")), "<i2"), "no endian")
  expect_error(mdimrefs_codec('{"compressor":{"id":"lzma"},"filters":null}', "<i2"),
               "unknown codec 'lzma'")
})

test_that("an array with an unknown codec is refused; the others still read", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  db <- make_byterefs(codec = '[{"name":"bytes","configuration":{"endian":"little"}},
                                {"name":"lzma"}]')
  expect_warning(st <- zaro(paste0("byterefs://", db), target = "local", verbose = FALSE),
                 "cannot read 1 array.*v: unknown codec 'lzma'")
  expect_error(zaro_read(st, "v", verbose = FALSE), "cannot read array 'v': unknown codec")
  expect_equal(as.vector(zaro_read(st, "x", verbose = FALSE)), 10 + 0.5 * 0:5)
})

test_that("a whole-object row (offset 0, size NULL) fetches the whole object", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  db <- make_byterefs()
  ## chunk (0,2) becomes a whole object: a file holding only that chunk
  obj <- tempfile(fileext = ".bin")
  writeBin(memCompress(writeBin(c(9L, 10L, 11L, 12L), raw(), size = 4L,
                                endian = "little"), "gzip"), obj)
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbExecute(con, "INSERT INTO sources VALUES(2, ?)", params = list(obj))
  DBI::dbExecute(con, "INSERT INTO placements VALUES('v', 2, 'v', '[0,0]', '[2,2]', '[0,4]')")
  DBI::dbExecute(con, 'DELETE FROM v WHERE dim_1 = 2')
  DBI::dbExecute(con, 'INSERT INTO v VALUES(2, 0, 0, 1, 0, NULL, NULL, NULL)')
  DBI::dbDisconnect(con)
  st <- zaro(paste0("byterefs://", db), target = "local", verbose = FALSE)
  p <- zaro_plan(st, "v", verbose = FALSE)
  expect_identical(c(p$offset[3], p$size[3]), c(0, NA))
  v <- zaro_read(st, "v", verbose = FALSE)
  expect_equal(as.vector(t(matrix(v, 2))), c(1:4, 9:10, 5:8, 11:12))
})
