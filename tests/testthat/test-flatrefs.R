## Flat refs stores (arrays.json + refs_<array>.parquet), read by row group

## v (y = 6, x = 4) int32, chunks (1, 2): a 6 x 2 chunk grid, 12 rows sorted
## by (dim_0, dim_1), 4 rows per row group -> 3 row groups. Chunk (2, 1) is
## inline, chunk (4, 0) is absent (present = 0). x is an affine coordinate,
## y an inline one. Paths are recorded as /old/<file>; target "local" remaps.
make_flat <- function(dir, codec = list(compressor = list(id = "zlib", level = 1L),
                                        filters = NULL),
                      rows_per_group = 4L, minmax = TRUE) {
  unlink(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE)
  v <- matrix(as.integer(seq_len(24)), 6, 4, byrow = TRUE)     # v[y, x]
  chunk <- function(i, j) memCompress(writeBin(v[i + 1L, j * 2L + 1:2], raw(),
                                               size = 4L, endian = "little"), "gzip")
  src <- file.path(dir, "source.bin")
  grid <- expand.grid(d1 = 0:1, d0 = 0:5)[, c("d0", "d1")]
  bytes <- lapply(seq_len(nrow(grid)), function(r) chunk(grid$d0[r], grid$d1[r]))
  writeBin(c(as.raw(1:5), unlist(bytes)), src)
  off <- 5 + c(0, cumsum(lengths(bytes)))[seq_along(bytes)]
  refs <- data.frame(dim_0 = grid$d0, dim_1 = grid$d1, present = 1L,
                     path = paste0("/old/", basename(src)), offset = off,
                     size = lengths(bytes))
  refs$data <- vector("list", nrow(refs))
  inl <- which(refs$dim_0 == 2 & refs$dim_1 == 1)
  refs$data[[inl]] <- bytes[[inl]]              # the encoded chunk, inline
  refs$path[inl] <- NA; refs$offset[inl] <- NA; refs$size[inl] <- NA
  abs <- which(refs$dim_0 == 4 & refs$dim_1 == 0)
  refs$present[abs] <- 0L; refs$path[abs] <- NA; refs$offset[abs] <- NA
  refs$size[abs] <- NA
  nanoparquet::write_parquet(refs, file.path(dir, "refs_v.parquet"),
    options = nanoparquet::parquet_options(num_rows_per_row_group = rows_per_group,
                                           write_minmax_values = minmax))
  aj <- list(
    format_version = "byterefs/0.1",
    meta = list(dataset = "test"),
    arrays = list(
      list(array = "v", kind = "data", storage = "referenced", dims = list("y", "x"),
           shape = list(6L, 4L), chunks = list(1L, 2L), dtype = "<i4", fill = "-1",
           codec = codec, attrs = list(units = "m")),
      list(array = "x", kind = "coord", storage = "affine", dims = list("x"),
           shape = list(4L), dtype = "<f8", affine = list(start = 10, step = 0.5)),
      list(array = "y", kind = "coord", storage = "inline", dims = list("y"),
           shape = list(6L), dtype = "<f8", values = as.list(c(-60, -50, -40, -30, -20, -10)))),
    remap = list(list(target = "local", prefix = "/old/",
                      replacement = paste0(normalizePath(dir), "/"))))
  writeLines(jsonlite::toJSON(aj, auto_unbox = TRUE, null = "null", digits = NA),
             file.path(dir, "arrays.json"))
  v[5, 1:2] <- -1L                       # the absent chunk reads as fill
  list(dir = dir, v = v)
}

test_that("a flat store reads as its arrays, chunk by chunk", {
  f <- make_flat(file.path(tempdir(), "flat-basic"))
  st <- zaro(paste0("flatrefs://", f$dir), target = "local", verbose = FALSE)
  expect_true(S7::S7_inherits(st, FlatRefsStore))
  v <- zaro_read(st, "v", verbose = FALSE)
  expect_equal(unname(unclass(v))[, ], f$v, ignore_attr = TRUE)
  expect_equal(as.vector(zaro_read(st, "x", verbose = FALSE)), 10 + 0.5 * 0:3)
  expect_equal(as.vector(zaro_read(st, "y", verbose = FALSE)), seq(-60, -10, 10))
  expect_identical(zaro_meta(st, "v", verbose = FALSE)@attributes$units, "m")

  p <- zaro_plan(st, "v", verbose = FALSE)
  expect_equal(as.vector(table(p$transport)[c("absent", "inline", "local")]),
               c(1, 1, 10))
})

test_that("only the row groups a selection needs are read", {
  f <- make_flat(file.path(tempdir(), "flat-prune"))
  st <- zaro(paste0("flatrefs://", f$dir), target = "local", verbose = FALSE)
  ## rows 0..1 are row group 0, rows 2..3 row group 1, rows 4..5 row group 2
  x <- zaro_read(st, "v", start = c(4, 0), count = c(2, 4), verbose = FALSE)
  expect_equal(unname(unclass(x))[, ], f$v[5:6, ], ignore_attr = TRUE)
  expect_identical(grep("^rg:", ls(st@cache), value = TRUE), "rg:v:2")
  pf <- st@cache[["pf:v"]]
  expect_equal(nrow(pf$rg), 3L)
  expect_equal(pf$stats[[1]]$min, c(0, 2, 4))
})

test_that("the row-group cache keeps the latest few", {
  f <- make_flat(file.path(tempdir(), "flat-lru"), rows_per_group = 2L)
  old <- options(zaro.flat_cache_row_groups = 2L)
  on.exit(options(old), add = TRUE)
  st <- zaro(paste0("flatrefs://", f$dir), target = "local", verbose = FALSE)
  for (i in 0:5) zaro_read(st, "v", start = c(i, 0), count = c(1, 4), verbose = FALSE)
  expect_setequal(grep("^rg:", ls(st@cache), value = TRUE), c("rg:v:4", "rg:v:5"))
  expect_equal(unname(unclass(zaro_read(st, "v", verbose = FALSE)))[, ], f$v,
               ignore_attr = TRUE)
})

test_that("no statistics: every row group, same values", {
  f <- make_flat(file.path(tempdir(), "flat-nostats"), minmax = FALSE)
  st <- zaro(paste0("flatrefs://", f$dir), target = "local", verbose = FALSE)
  x <- zaro_read(st, "v", start = c(4, 0), count = c(2, 4), verbose = FALSE)
  expect_equal(unname(unclass(x))[, ], f$v[5:6, ], ignore_attr = TRUE)
  expect_length(grep("^rg:", ls(st@cache)), 3L)
})

test_that("remap targets, and unknown codecs refuse the array", {
  f <- make_flat(file.path(tempdir(), "flat-remap"))
  expect_warning(st <- zaro(paste0("flatrefs://", f$dir), target = "nope",
                            verbose = FALSE), "no remap rules for target 'nope'")
  expect_true(all(startsWith(stats::na.omit(zaro_plan(st, "v", verbose = FALSE)$path),
                             "/old/")))

  g <- make_flat(file.path(tempdir(), "flat-refused"),
                 codec = list(list(name = "bytes", configuration = list(endian = "little")),
                              list(name = "lzma")))
  expect_warning(st <- zaro(paste0("flatrefs://", g$dir), target = "local",
                            verbose = FALSE), "v: unknown codec 'lzma'")
  expect_identical(names(st@refused), "v")
  expect_error(zaro_read(st, "v", verbose = FALSE), "cannot read array 'v'")
  expect_equal(as.vector(zaro_read(st, "x", verbose = FALSE)), 10 + 0.5 * 0:3)
})

test_that("a flat store over HTTP: footer and row groups by range", {
  f <- make_flat(file.path(tempdir(), "flat-http-src"))
  srv <- local_range_server(f$dir)
  on.exit(srv$stop(), add = TRUE)
  st <- zaro(paste0("flatrefs://", srv$url), target = "local", verbose = FALSE)
  x <- zaro_read(st, "v", start = c(2, 0), count = c(2, 4), verbose = FALSE)
  expect_equal(unname(unclass(x))[, ], f$v[3:4, ], ignore_attr = TRUE)
  expect_identical(grep("^rg:", ls(st@cache), value = TRUE), "rg:v:1")
})

test_that("footer rewrite: long list headers and the num_rows field", {
  df <- data.frame(dim_0 = rep(0:39, each = 5), k = seq_len(200))
  f <- tempfile(fileext = ".parquet")
  nanoparquet::write_parquet(df, f, options = nanoparquet::parquet_options(
    num_rows_per_row_group = 5L))                    # 40 row groups (> 15)
  pf <- pq_footer(f)
  expect_equal(nrow(pf$rg), 40L)
  got <- pq_read_row_groups(pf, c(3L, 17L, 39L))
  expect_equal(got$k, c(16:20, 86:90, 196:200))
  expect_equal(pq_int_stats(pf, "dim_0")$min, 0:39)
  sub <- pq_footer_subset(pf$foot, 0:19, 100)
  tf <- pq_sparse(pf$size, list(), list(), sub)
  on.exit(unlink(tf), add = TRUE)
  m <- nanoparquet::read_parquet_metadata(tf)
  expect_equal(m$file_meta_data$num_rows, 100)
  expect_equal(nrow(m$row_groups), 20L)
})

test_that("a server that ignores suffix ranges still gives the footer", {
  ## the test server answers bytes=-n with the whole file under /nosuffix/
  root <- file.path(tempdir(), "flat-nosuffix-root")
  f <- make_flat(file.path(root, "nosuffix"))
  srv <- local_range_server(root)
  on.exit(srv$stop(), add = TRUE)
  tl <- pq_tail(paste0(srv$url, "/nosuffix/refs_v.parquet"), 100)
  expect_equal(tl$size, file.size(file.path(f$dir, "refs_v.parquet")))
  expect_length(tl$tail, 100L)
  st <- zaro(paste0("flatrefs://", srv$url, "/nosuffix"), target = "local",
             verbose = FALSE)
  expect_equal(unname(unclass(zaro_read(st, "v", verbose = FALSE)))[, ], f$v,
               ignore_attr = TRUE)
})

test_that("a selection over every row group holds at most the cache size", {
  f <- make_flat(file.path(tempdir(), "flat-span"), rows_per_group = 2L)  # 6 row groups
  old <- options(zaro.flat_cache_row_groups = 2L)
  on.exit(options(old), add = TRUE)
  st <- zaro(paste0("flatrefs://", f$dir), target = "local", verbose = FALSE)
  x <- zaro_read(st, "v", start = c(0, 0), count = c(6, 2), verbose = FALSE)
  expect_equal(unname(unclass(x))[, ], f$v[, 1:2], ignore_attr = TRUE)
  expect_lte(length(grep("^rg:", ls(st@cache))), 2L)
})
