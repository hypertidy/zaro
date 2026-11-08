## Placement-first chunk lookup (store_refs on a byterefs store) against
## filtering the refs_<array> view, for every key of a small store.

## A byterefs/0.1 store with the spec's tables and view (as raad.virtualize
## mr_init() and mr_view_sql() write them). Array sst, dims (time, y, x),
## shape (5, 4, 6), chunks (1, 2, 3): a 5 x 2 x 2 chunk grid.
##   time 0: day1.nc, the whole slab; one chunk absent, one inline
##   time 1: two.nc, a two-day file read from its second day (src_offset 1)
##   time 2: two tiles side by side on x, t2a.tif and t2b.tif; one t2b
##           chunk is a whole object (offset 0, size NULL)
##   time 3, 4: no placement (absent)
## Remap target 'public' with a longer prefix for two.nc.
make_lookup_store <- function() {
  db <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con))
  x <- function(sql, ...) DBI::dbExecute(con, sql, ...)
  x("CREATE TABLE meta(key TEXT PRIMARY KEY, value TEXT) WITHOUT ROWID")
  x("INSERT INTO meta VALUES('format_version', 'byterefs/0.1')")
  x("CREATE TABLE sources(source_id INTEGER PRIMARY KEY, source TEXT NOT NULL UNIQUE,
     size BIGINT, mtime BIGINT, etag TEXT)")
  x("INSERT INTO sources(source_id, source) VALUES (3, '/rdsi/day1.nc'),
     (7, '/rdsi/two.nc'), (8, '/rdsi/t2a.tif'), (9, '/rdsi/t2b.tif')")
  x('CREATE TABLE arrays(array TEXT PRIMARY KEY, kind TEXT NOT NULL, storage TEXT NOT NULL,
     dims TEXT, shape TEXT, chunks TEXT, dtype TEXT, fill TEXT, codec TEXT, attrs TEXT,
     affine TEXT, "values" BLOB) WITHOUT ROWID')
  x("INSERT INTO arrays(array, kind, storage, dims, shape, chunks, dtype, fill, codec)
     VALUES ('sst', 'data', 'referenced', '[\"time\",\"y\",\"x\"]', '[5,4,6]', '[1,2,3]',
             '<i2', '-32768', '{\"compressor\":{\"id\":\"zlib\",\"level\":1},\"filters\":null}')")
  x("CREATE TABLE placements(array TEXT NOT NULL, source_id INTEGER NOT NULL,
     source_array TEXT NOT NULL, src_offset TEXT NOT NULL, count TEXT NOT NULL,
     dest TEXT NOT NULL, PRIMARY KEY (array, dest)) WITHOUT ROWID")
  x("INSERT INTO placements VALUES
     ('sst', 3, 'sst', '[0,0,0]', '[1,4,6]', '[0,0,0]'),
     ('sst', 7, 'sst', '[1, 0, 0]', '[1, 4, 6]', '[1, 0, 0]'),
     ('sst', 8, 'band:1', '[0,0,0]', '[1,4,3]', '[2,0,0]'),
     ('sst', 9, 'band:1', '[0,0,0]', '[1,4,3]', '[2,0,3]')")
  x("CREATE TABLE remap(target TEXT NOT NULL, prefix TEXT NOT NULL, replacement TEXT NOT NULL,
     PRIMARY KEY (target, prefix)) WITHOUT ROWID")
  x("INSERT INTO remap VALUES ('public', '/rdsi/', 'https://example.org/data/'),
     ('public', '/rdsi/two', 's3://bucket/two')")
  x('CREATE TABLE sst(source_id INTEGER NOT NULL, dim_0 INTEGER NOT NULL,
     dim_1 INTEGER NOT NULL, dim_2 INTEGER NOT NULL, present INTEGER NOT NULL DEFAULT 1,
     "offset" BIGINT, size BIGINT, info TEXT, data BLOB,
     PRIMARY KEY (source_id, dim_0, dim_1, dim_2)) WITHOUT ROWID')
  rows <- rbind(
    ## day1: (0,0,0) present, (0,0,1) absent, (0,1,0) inline, (0,1,1) present
    data.frame(s = 3, d0 = 0, d1 = c(0, 0, 1, 1), d2 = c(0, 1, 0, 1),
               present = c(1, 0, 1, 1), offset = c(100, NA, NA, 300),
               size = c(10, NA, NA, 30)),
    ## two.nc: both days; only day 1 is placed
    data.frame(s = 7, d0 = rep(0:1, each = 4), d1 = rep(c(0, 0, 1, 1), 2),
               d2 = rep(c(0, 1, 0, 1), 2), present = 1,
               offset = 5000 + 0:7 * 100, size = 50),
    ## tiles: x chunk 0 of each source
    data.frame(s = 8, d0 = 0, d1 = 0:1, d2 = 0, present = 1,
               offset = c(10, 20), size = 5),
    data.frame(s = 9, d0 = 0, d1 = 0:1, d2 = 0, present = 1,
               offset = c(0, 40), size = c(NA, 6)))
  for (r in seq_len(nrow(rows))) {
    v <- rows[r, ]
    x('INSERT INTO sst VALUES(?, ?, ?, ?, ?, ?, ?, NULL, ?)',
      params = list(v$s, v$d0, v$d1, v$d2, v$present, v$offset, v$size,
                    list(if (v$s == 3 && v$d1 == 1 && v$d2 == 0) as.raw(1:4) else NULL)))
  }
  ## the view, as the spec defines it (mr_view_sql() with the verbatim path)
  je <- function(col, k) sprintf("CAST(json_extract(p.%s, '$[%d]') AS INTEGER)", col, k)
  ch <- c(1L, 2L, 3L); i <- 0:2
  glob <- sprintf("c.dim_%d - %s / %d + %s / %d AS dim_%d", i, je("src_offset", i), ch,
                  je("dest", i), ch, i)
  inside <- sprintf("c.dim_%d >= %s / %d AND c.dim_%d < (%s + %s + %d - 1) / %d",
                    i, je("src_offset", i), ch, i, je("src_offset", i), je("count", i), ch, ch)
  x(sprintf('CREATE VIEW refs_sst AS SELECT %s, c.present,
       CASE WHEN c."offset" IS NULL THEN NULL ELSE s.source END AS path,
       c."offset", c.size, c.info, c.data
     FROM sst c JOIN placements p ON p.array = \'sst\' AND p.source_id = c.source_id
                JOIN sources s ON s.source_id = c.source_id
     WHERE %s ORDER BY dim_0, dim_1, dim_2',
    paste(glob, collapse = ", "), paste(inside, collapse = " AND ")))
  db
}

## the reference: filter the view for one key, as zaro did before
view_ref <- function(store, key) {
  idx <- as.integer(strsplit(sub("^.*/", "", key), ".", fixed = TRUE)[[1]])
  r <- DBI::dbGetQuery(store@con, sprintf(
    'SELECT present, path, "offset", size, data FROM refs_sst WHERE %s',
    paste(sprintf("dim_%d = %d", seq_along(idx) - 1L, idx), collapse = " AND ")))
  out <- list(path = NA_character_, offset = NA_real_, size = NA_real_,
              transport = "absent", data = NULL)
  if (!nrow(r) || r$present[1] == 0L) return(out)
  if (is.na(r$offset[1])) return(modifyList(out, list(transport = "inline",
                                                      data = as.raw(r$data[[1]]))))
  p <- mdimrefs_path(store, r$path[1])
  list(path = p, offset = as.numeric(r$offset[1]), size = as.numeric(r$size[1]),
       transport = url_transport(p), data = NULL)
}

all_keys <- function() {
  g <- expand.grid(d2 = 0:1, d1 = 0:1, d0 = 0:4)
  sprintf("sst/%d.%d.%d", g$d0, g$d1, g$d2)
}

test_that("placement-first lookup equals filtering the view, every key", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  db <- make_lookup_store()
  for (target in c("", "public")) {
    st <- zaro(paste0("byterefs://", db), target = target, verbose = FALSE)
    keys <- all_keys()
    ## one batch, in reverse order: rows come back in request order
    got <- store_refs(st, rev(keys))
    for (j in seq_along(keys)) {
      k <- rev(keys)[j]
      want <- view_ref(st, k)
      expect_identical(got$key[j], k)
      expect_identical(got$transport[j], want$transport, info = k)
      expect_identical(got$path[j], want$path, info = k)
      expect_identical(got$offset[j], want$offset, info = k)
      expect_identical(got$size[j], want$size, info = k)
      expect_identical(got$data[[j]], want$data, info = k)
      ## and one key at a time
      one <- store_refs(st, k)
      expect_identical(one[setdiff(names(one), "data")],
                       got[j, setdiff(names(got), "data")], ignore_attr = TRUE)
    }
    ## every state occurs: byte range, absent, inline, whole object
    expect_setequal(unique(got$transport),
                    c("absent", "inline", if (nzchar(target)) c("http", "s3") else "local"))
    DBI::dbDisconnect(st@con)
  }
})

test_that("the lookup reads the right rows of each source", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  st <- zaro(paste0("byterefs://", make_lookup_store()), target = "public", verbose = FALSE)
  r <- store_refs(st, c("sst/1.1.1", "sst/2.0.1", "sst/2.0.0", "sst/2.1.1", "sst/0.0.1",
                        "sst/3.0.0", "sst/0.1.0"))
  ## two.nc from its second day: within-source dim_0 1, the 8th row
  expect_identical(r$offset[1], 5700)
  expect_identical(r$path[1], "s3://bucket/two.nc")       # longest prefix wins
  ## side-by-side tiles: x chunk 1 is t2b's chunk 0; the whole object
  expect_identical(r$path[2], "https://example.org/data/t2b.tif")
  expect_identical(c(r$offset[2], r$size[2]), c(0, NA))
  expect_identical(r$path[3], "https://example.org/data/t2a.tif")
  expect_identical(c(r$offset[4], r$size[4]), c(40, 6))
  ## absent row, no placement, inline
  expect_identical(r$transport[5:7], c("absent", "absent", "inline"))
  expect_identical(r$data[[7]], as.raw(1:4))
  DBI::dbDisconnect(st@con)
})

test_that("keys outside the array, or not chunk keys, are absent", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  st <- zaro(paste0("byterefs://", make_lookup_store()), verbose = FALSE)
  r <- store_refs(st, c("sst/5.0.0", "sst/0.2.0", "sst/0.0.2", "sst/0.0.0", "sst/0.0",
                        "nope/0.0.0", "sst/.zarray"))
  expect_identical(r$transport, c("absent", "absent", "absent", "local",
                                  "absent", "absent", "absent"))
  ## directly: keys no placement holds are missing from the result
  pl <- mdimrefs_placements(st@con, "sst", c(1, 2, 3))
  got <- mdimrefs_chunks(st@con, "sst",
                         rbind(c(9L, 9L, 9L), c(1L, 1L, 1L), c(-1L, 0L, 0L)), pl)
  expect_identical(got$k, 2L)
  DBI::dbDisconnect(st@con)
})

test_that("a large batch is split and keeps request order", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  st <- zaro(paste0("byterefs://", make_lookup_store()), verbose = FALSE)
  idx <- as.matrix(expand.grid(0:1, 0:1, 0:2)[c(5, 1, 9, 3), ])
  storage.mode(idx) <- "integer"
  pl <- mdimrefs_placements(st@con, "sst", c(1, 2, 3))
  a <- mdimrefs_chunks(st@con, "sst", idx, pl)
  b <- mdimrefs_chunks(st@con, "sst", idx, pl, batch = 1L)
  expect_identical(a$k, b$k)
  expect_identical(a$offset, b$offset)
  expect_false(is.unsorted(a$k))
  DBI::dbDisconnect(st@con)
})

test_that("a store without placements falls back to the view", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  db <- make_lookup_store()
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  want <- {
    st <- zaro(paste0("byterefs://", db), target = "public", verbose = FALSE)
    on.exit(DBI::dbDisconnect(st@con))
    store_refs(st, all_keys())
  }
  ## materialise the view, then drop placements
  DBI::dbExecute(con, "CREATE TABLE refs_tmp AS SELECT * FROM refs_sst")
  DBI::dbExecute(con, "DROP VIEW refs_sst")
  DBI::dbExecute(con, "DROP TABLE placements")
  DBI::dbExecute(con, "ALTER TABLE refs_tmp RENAME TO refs_sst")
  DBI::dbDisconnect(con)
  st2 <- zaro(paste0("byterefs://", db), target = "public", verbose = FALSE)
  got <- store_refs(st2, all_keys())
  expect_identical(got, want)
  DBI::dbDisconnect(st2@con)
})

test_that("size NULL with an offset other than 0 is an invalid row (decision 0008)", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  db <- make_lookup_store()
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbExecute(con, 'UPDATE sst SET size = NULL WHERE source_id = 9 AND dim_1 = 1')
  DBI::dbDisconnect(con)
  st <- zaro(paste0("byterefs://", db), verbose = FALSE)
  expect_error(store_refs(st, "sst/2.1.1"),
               "invalid chunk row for 'sst/2.1.1': size is NULL with offset 40")
  ## offset 0 and size NULL is the whole object
  expect_identical(store_refs(st, "sst/2.0.1")$size, NA_real_)
  DBI::dbDisconnect(st@con)
})
