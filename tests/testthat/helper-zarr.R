# Write a small Zarr array to a local directory, for tests.
#
# `x` is given in R dim order (the order zaro_array() returns); for C order
# the Zarr shape is rev(dim(x)). `chunk` is in R dim order too. Edge chunks
# are padded with `fill`. Chunks listed in `skip` (0-based, R order) are not
# written. Supports V2 (compressor null or zlib) and V3 (bytes codec only).
write_test_zarr <- function(path, x, chunk, version = 2L, order = "C",
                            compressor = "zlib", fill = -1,
                            dim_names = NULL, skip = list()) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  d <- dim(x)
  rev_c <- identical(order, "C")
  zshape <- if (rev_c) rev(d) else d
  zchunk <- if (rev_c) rev(chunk) else chunk
  is_int <- is.integer(x)
  size <- if (is_int) 4L else 8L
  dtype2 <- if (is_int) "<i4" else "<f8"
  dtype3 <- if (is_int) "int32" else "float64"
  zdims <- if (!is.null(dim_names) && rev_c) rev(dim_names) else dim_names

  jarr <- function(v) paste0("[", paste(v, collapse = ", "), "]")
  jstr <- function(v) paste0("[", paste0('"', v, '"', collapse = ", "), "]")
  if (version == 2L) {
    comp <- if (compressor == "zlib") '{"id": "zlib", "level": 1}' else "null"
    writeLines(sprintf(
      '{"zarr_format": 2, "shape": %s, "chunks": %s, "dtype": "%s", "compressor": %s, "fill_value": %s, "filters": null, "order": "%s"}',
      jarr(zshape), jarr(zchunk), dtype2, comp, fill, order),
      file.path(path, ".zarray"))
    if (!is.null(zdims)) {
      writeLines(sprintf('{"_ARRAY_DIMENSIONS": %s}', jstr(zdims)),
                 file.path(path, ".zattrs"))
    }
  } else {
    dn <- if (is.null(zdims)) "" else sprintf(', "dimension_names": %s', jstr(zdims))
    writeLines(sprintf(paste0(
      '{"zarr_format": 3, "node_type": "array", "shape": %s, "data_type": "%s", ',
      '"chunk_grid": {"name": "regular", "configuration": {"chunk_shape": %s}}, ',
      '"chunk_key_encoding": {"name": "default", "configuration": {"separator": "/"}}, ',
      '"codecs": [{"name": "bytes", "configuration": {"endian": "little"}}], ',
      '"fill_value": %s%s}'),
      jarr(zshape), dtype3, jarr(zchunk), fill, dn),
      file.path(path, "zarr.json"))
  }

  grid <- lapply(ceiling(d / chunk) - 1L, function(n) seq.int(0L, n))
  cc_all <- as.matrix(expand.grid(grid, KEEP.OUT.ATTRS = FALSE))
  for (r in seq_len(nrow(cc_all))) {
    cc <- as.integer(cc_all[r, ])
    if (any(vapply(skip, function(s) all(s == cc), logical(1)))) next
    st <- cc * chunk + 1L
    ext <- pmin(chunk, d - st + 1L)
    region <- do.call(`[`, c(list(x), Map(function(a, n) seq.int(a, length.out = n), st, ext),
                             list(drop = FALSE)))
    padded <- array(if (is_int) as.integer(fill) else as.double(fill), chunk)
    padded <- do.call(`[<-`, c(list(padded), lapply(ext, seq_len), list(value = region)))
    bytes <- writeBin(as.vector(padded), raw(), size = size, endian = "little")
    if (version == 2L && compressor == "zlib") {
      bytes <- memCompress(bytes, type = "gzip")
    }
    zcc <- if (rev_c) rev(cc) else cc
    if (version == 2L) {
      key <- file.path(path, paste(zcc, collapse = "."))
    } else {
      key <- file.path(path, "c", paste(zcc, collapse = "/"))
      dir.create(dirname(key), recursive = TRUE, showWarnings = FALSE)
    }
    writeBin(bytes, key)
  }
  invisible(path)
}


# Write a sharded Zarr V3 array (sharding_indexed, inner codec "bytes"
# little-endian float64) to a local directory, for tests.
#
# `x`, `shard` and `inner` are in R dim order (Zarr shape is the reverse).
# `empty` lists inner chunks (0-based global inner grid index, R order)
# written as empty index entries; `missing` lists shards (0-based shard
# grid index, R order) not written at all. The index is at `index_location`
# ("end" or "start"), with a crc32c codec (4 zero bytes, not verified by
# zaro) when `crc` is TRUE. Edge shards and chunks are padded with `fill`.
write_sharded_zarr <- function(path, x, shard, inner, index_location = "end",
                               crc = TRUE, fill = -1, empty = list(),
                               missing = list(), dim_names = NULL) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  d <- dim(x)
  per <- shard %/% inner
  jarr <- function(v) paste0("[", paste(v, collapse = ", "), "]")
  jstr <- function(v) paste0("[", paste0('"', v, '"', collapse = ", "), "]")
  idx_codecs <- if (crc) {
    '[{"name": "bytes", "configuration": {"endian": "little"}}, {"name": "crc32c"}]'
  } else '[{"name": "bytes", "configuration": {"endian": "little"}}]'
  dn <- if (is.null(dim_names)) "" else sprintf(', "dimension_names": %s', jstr(rev(dim_names)))
  writeLines(sprintf(paste0(
    '{"zarr_format": 3, "node_type": "array", "shape": %s, "data_type": "float64", ',
    '"chunk_grid": {"name": "regular", "configuration": {"chunk_shape": %s}}, ',
    '"chunk_key_encoding": {"name": "default", "configuration": {"separator": "/"}}, ',
    '"codecs": [{"name": "sharding_indexed", "configuration": {"chunk_shape": %s, ',
    '"codecs": [{"name": "bytes", "configuration": {"endian": "little"}}], ',
    '"index_codecs": %s, "index_location": "%s"}}], "fill_value": %s%s}'),
    jarr(rev(d)), jarr(rev(shard)), jarr(rev(inner)), idx_codecs, index_location,
    fill, dn), file.path(path, "zarr.json"))

  same <- function(a, lst) any(vapply(lst, function(s) all(s == a), logical(1)))
  sgrid <- lapply(ceiling(d / shard) - 1L, function(n) seq.int(0L, n))
  sall <- as.matrix(expand.grid(sgrid, KEEP.OUT.ATTRS = FALSE))
  ff16 <- as.raw(rep(0xff, 16))
  u64 <- function(v) {               # small non-negative values only
    writeBin(as.integer(c(rbind(v, 0))), raw(), size = 4L, endian = "little")
  }
  for (r in seq_len(nrow(sall))) {
    sc <- as.integer(sall[r, ])
    if (same(sc, missing)) next
    ## inner chunks of this shard in C order of the Zarr dims: the last
    ## Zarr dim (first R dim) varies fastest
    igrid <- lapply(per - 1L, function(n) seq.int(0L, n))
    iall <- as.matrix(expand.grid(igrid, KEEP.OUT.ATTRS = FALSE))   # R order, first fastest
    payload <- list()
    entries <- list()
    for (q in seq_len(nrow(iall))) {
      ic <- sc * per + as.integer(iall[q, ])     # global inner index, R order
      st <- ic * inner + 1L
      if (any(st > d) || same(ic, empty)) {
        entries[[q]] <- NULL
        next
      }
      ext <- pmin(inner, d - st + 1L)
      region <- do.call(`[`, c(list(x), Map(function(a, n) seq.int(a, length.out = n), st, ext),
                               list(drop = FALSE)))
      padded <- array(as.double(fill), inner)
      padded <- do.call(`[<-`, c(list(padded), lapply(ext, seq_len), list(value = region)))
      payload[[q]] <- writeBin(as.vector(padded), raw(), size = 8L, endian = "little")
    }
    n_inner <- nrow(iall)
    isize <- 16 * n_inner + if (crc) 4 else 0
    pos <- if (index_location == "start") isize else 0
    index <- raw()
    body <- raw()
    for (q in seq_len(n_inner)) {
      b <- if (q <= length(payload)) payload[[q]] else NULL
      if (is.null(b)) {
        index <- c(index, ff16)
      } else {
        index <- c(index, u64(c(pos, length(b))))
        body <- c(body, b)
        pos <- pos + length(b)
      }
    }
    if (crc) index <- c(index, as.raw(c(0, 0, 0, 0)))
    bytes <- if (index_location == "start") c(index, body) else c(body, index)
    key <- do.call(file.path, as.list(c(path, "c", rev(sc))))
    dir.create(dirname(key), recursive = TRUE, showWarnings = FALSE)
    writeBin(bytes, key)
  }
  invisible(path)
}
