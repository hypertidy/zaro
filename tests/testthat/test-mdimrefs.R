test_that("mdimrefs:// reads chunks through the refs_ view", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("gdalraster")

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
  DBI::dbExecute(con, "INSERT INTO meta VALUES('format_version', 'mdim-refs/0.2')")
  DBI::dbExecute(con, "CREATE TABLE sources(source_id INTEGER PRIMARY KEY, source TEXT)")
  DBI::dbExecute(con, "INSERT INTO sources VALUES(1, ?)", params = list(paste0("/old/", basename(src))))
  DBI::dbExecute(con, "CREATE TABLE remap(target TEXT, prefix TEXT, replacement TEXT)")
  DBI::dbExecute(con, "INSERT INTO remap VALUES('local', '/old/', ?)",
                 params = list(paste0(dirname(src), "/")))
  DBI::dbExecute(con, 'CREATE TABLE arrays(array TEXT PRIMARY KEY, kind TEXT, storage TEXT,
    dims TEXT, shape TEXT, chunks TEXT, dtype TEXT, fill TEXT, codec TEXT, attrs TEXT,
    affine TEXT, "values" BLOB)')
  DBI::dbExecute(con, "INSERT INTO arrays VALUES('v', 'data', 'referenced', '[\"y\",\"x\"]',
    '[2,6]', '[2,2]', '<i4', '-1', '{\"compressor\":{\"id\":\"zlib\",\"level\":1},\"filters\":null}',
    '{\"units\":\"m\"}', NULL, NULL)")
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

  st <- zaro(paste0("mdimrefs://", db), target = "local", verbose = FALSE)
  expect_true(".zmetadata" %in% zaro_list(st))
  v <- zaro_read(st, "v", verbose = FALSE)
  expect_equal(as.vector(t(matrix(v, 2))), c(1:4, -1L, -1L, 5:8, -1L, -1L))
  expect_equal(as.vector(zaro_read(st, "x", verbose = FALSE)), 10 + 0.5 * 0:5)
})
