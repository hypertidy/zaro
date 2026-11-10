# Icechunk repositories through icechunkr (IcechunkStore, icechunk.R)
#
# Built on icechunkr::ic_example_repo(): /from_bin (four virtual chunks in a
# local file), /from_nc (one virtual chunk at an offset), /small (inline
# chunks, second commit; tag v1 is the first commit). Values of the virtual
# arrays are (0:3071) / 4, float32, C order.

skip_if_not_installed("icechunkr")
skip_if_not_installed("altarr")

ic_ex <- function() {
  ex <- icechunkr::ic_example_repo()
  ex$repo <- icechunkr::ic_repo_open(ex$path, authorize = ex$authorize)
  ex
}

test_that("an Icechunk snapshot reads as a zaro store", {
  ex <- ic_ex()
  store <- zaro_icechunk(ex$repo, verbose = FALSE)
  expect_true(S7::S7_inherits(store, IcechunkStore))
  expect_match(store@snapshot, "^[0-9A-Z]{20}$")

  m <- zaro_meta(store, "from_bin", verbose = FALSE)
  expect_equal(m@shape, c(64, 48))

  x <- zaro_array(store, "from_bin", verbose = FALSE)
  expect_equal(dim(x), c(48L, 64L))
  expect_equal(as.vector(x[, ]), ex$values)
  expect_equal(x[3, 2], ex$values[48 + 3])

  nc <- zaro_array(store, "from_nc", verbose = FALSE)
  expect_equal(as.vector(nc[, ]), ex$values)

  # inline chunks (C order 4 x 6 of 1:24)
  s <- zaro_array(store, "small", verbose = FALSE)
  expect_equal(unname(t(s[, ])), matrix(as.double(1:24), 4, 6, byrow = TRUE))
})

test_that("chunks come from the manifest: inline, store, absent", {
  ex <- ic_ex()
  store <- zaro_icechunk(ex$repo, verbose = FALSE)
  r <- store_refs(store, c("small/c/0/0", "from_bin/c/1/0", "from_bin/c/9/0",
                           "zarr.json"))
  expect_equal(r$transport, c("inline", "store", "absent", "store"))
  expect_length(r$data[[1L]], 48L)
})

test_that("tag, snapshot and branch pick the version", {
  ex <- ic_ex()
  # (small's y and x differ from the other arrays', so zaro_dataset() warns)
  arrays <- function(st) names(suppressWarnings(zaro_dataset(st, verbose = FALSE))$arrays)
  v1 <- zaro_icechunk(ex$repo, tag = "v1", verbose = FALSE)
  expect_setequal(arrays(v1), c("from_bin", "from_nc"))
  main <- zaro_icechunk(ex$repo, verbose = FALSE)
  expect_setequal(arrays(main), c("from_bin", "from_nc", "small"))
  again <- zaro_icechunk(ex$repo, snapshot = v1@snapshot, verbose = FALSE)
  expect_identical(again@snapshot, v1@snapshot)

  # icechunk:// with a query, and the objects themselves, through zaro()
  u <- zaro(paste0("icechunk://", ex$path, "?tag=v1"),
            authorize = ex$authorize, verbose = FALSE)
  expect_identical(u@snapshot, v1@snapshot)
  s <- icechunkr::ic_session_readonly(ex$repo, tag = "v1")
  expect_identical(zaro(s, verbose = FALSE)@snapshot, v1@snapshot)
  expect_identical(zaro(ex$repo, tag = "v1", verbose = FALSE)@snapshot,
                   v1@snapshot)
})

test_that("a lazy array stays on its snapshot when the branch moves", {
  ex <- ic_ex()
  store <- zaro_icechunk(ex$repo, verbose = FALSE)
  x <- zaro_array(store, "small", verbose = FALSE)

  w <- icechunkr::ic_session_writable(ex$repo, "main")
  icechunkr::ic_set(w, "small/c/0/0",
                    writeBin(rep(-1, 6), raw(), size = 8, endian = "little"))
  icechunkr::ic_commit(w, "overwrite one chunk")

  expect_equal(unname(t(x[, ])), matrix(as.double(1:24), 4, 6, byrow = TRUE))
  now <- zaro_array(zaro_icechunk(ex$repo, verbose = FALSE), "small",
                    verbose = FALSE)
  expect_equal(unname(now[1:3, 1:2]), matrix(-1, 3, 2))
})

test_that("direct = TRUE fetches authorised virtual chunks with zaro", {
  ex <- ic_ex()
  store <- zaro_icechunk(ex$repo, direct = TRUE, verbose = FALSE)
  r <- store_refs(store, c("from_bin/c/1/0", "from_nc/c/0/0"))
  expect_equal(r$transport, c("local", "local"))
  expect_equal(r$offset, c(3072, 2048))
  expect_equal(as.vector(zaro_array(store, "from_bin", verbose = FALSE)[, ]),
               ex$values)

  # no authorisation: nothing is fetched around icechunk
  bare <- icechunkr::ic_repo_open(ex$path)
  store <- zaro_icechunk(bare, direct = TRUE, verbose = FALSE)
  expect_equal(store_refs(store, "from_bin/c/1/0")$transport, "store")

  # a session alone can't say what is authorised
  s <- icechunkr::ic_session_readonly(ex$repo, branch = "main")
  expect_error(zaro_icechunk(s, direct = TRUE), "needs the repository")
})

test_that("writable sessions are refused", {
  ex <- ic_ex()
  w <- icechunkr::ic_session_writable(ex$repo, "main")
  expect_error(zaro_icechunk(w, verbose = FALSE), "read-only")
})

test_that("native chunks with V3 numcodecs.shuffle + numcodecs.zlib decode", {
  ex <- ic_ex()
  w <- icechunkr::ic_session_writable(ex$repo, "main")
  icechunkr::ic_set(w, "nc4/zarr.json", charToRaw(
    '{"zarr_format":3,"node_type":"array","shape":[200,300],"data_type":"int16",
      "chunk_grid":{"name":"regular","configuration":{"chunk_shape":[100,300]}},
      "chunk_key_encoding":{"name":"default","configuration":{"separator":"/"}},
      "fill_value":0,"codecs":[{"name":"bytes","configuration":{"endian":"little"}},
      {"name":"numcodecs.shuffle","configuration":{"elementsize":2}},
      {"name":"numcodecs.zlib","configuration":{"level":4}}],
      "attributes":{},"dimension_names":["y","x"]}'))
  set.seed(1)
  v <- matrix(sample(-3000:3000, 60000, replace = TRUE), 200, 300)
  shuffle <- function(b, n) as.vector(t(matrix(b, nrow = n)))
  for (i in 0:1) {
    blk <- as.vector(t(v[i * 100 + 1:100, ]))
    b <- writeBin(as.integer(blk), raw(), size = 2, endian = "little")
    icechunkr::ic_set(w, sprintf("nc4/c/%d/0", i),
                      memCompress(shuffle(b, 2L), "gzip"))
  }
  icechunkr::ic_commit(w, "netCDF-4 style chunks")

  store <- zaro_icechunk(ex$repo, verbose = FALSE)
  expect_true(all(store_refs(store, c("nc4/c/0/0", "nc4/c/1/0"))$transport ==
                  "store"))
  x <- zaro_array(store, "nc4", verbose = FALSE)
  expect_equal(unname(t(x[, ])), v)
})
