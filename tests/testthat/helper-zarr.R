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
