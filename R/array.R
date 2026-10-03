# array.R -- lazy arrays via altarr
#
# zaro_array()   open a Zarr array as a lazy R array (ALTREP, via altarr)
#
# zaro already reads chunks one at a time (fetch_chunk() + decode_chunk()),
# which is exactly the shape of altarr's fetch contract: given a matrix of
# 0-based chunk coordinates, return a list of chunk vectors. The array is
# presented in R's dim order, i.e. the Zarr shape reversed for C-order
# arrays. C-order bytes are already column-major for the reversed shape, so
# no aperm() is ever done.


#' Open a Zarr array as a lazy R array
#'
#' Returns an ordinary R array (a double, integer or logical vector with a
#' `dim` attribute and no class) whose values are read from the store chunk
#' by chunk, only when R asks for them. It is an ALTREP object built by
#' [altarr::altarr()], with zaro's chunk fetch and codec pipeline behind it.
#' Nothing is read when the array is created beyond the metadata.
#'
#' Subsetting with `x[i]`, `x[cbind(i, j, k)]` and
#' `altarr::altarr_extract(x, i, j, k)` plans the chunks it needs and fetches
#' them in one batch; `sum()`, `mean()`, `min()` and `max()` stream the chunk
#' grid. See `?altarr::altarr_contract` for what base R does with a lazy
#' array, and [altarr::altarr_stats()] to count fetches.
#'
#' @section Dimension order:
#' A C-order Zarr array of shape `(s1, ..., sn)` (all Zarr V3 arrays, and V2
#' arrays with `"order": "C"`) is returned with R `dim` `c(sn, ..., s1)`, and
#' the dimension names reversed to match. This is the order the bytes are
#' stored in, so a chunk is used as decoded. For a typical
#' `(time, lat, lon)` array, `x[lon, lat, time]` is the R subscript. V2
#' arrays with `"order": "F"` keep their shape. This is the transpose of
#' what [zaro_read()] returns.
#'
#' @section Missing chunks and types:
#' Chunks absent from the store read as the array's `fill_value`. Integer
#' types up to 32 bits give an integer array (`uint32` and 64-bit integers
#' give double, as in [zaro_read()]), floats give double, `bool` gives
#' logical. No CF packing (`scale_factor`, `add_offset`, `_FillValue`) is
#' applied.
#'
#' @section Saving:
#' `saveRDS()` on the result writes the recipe (shape, chunk shape and the
#' fetch function), not the values. Stores backed by Arrow or GDAL hold
#' external pointers, so a saved array does not read in a new R session;
#' re-open it with `zaro_array()`.
#'
#' @param store a store object from [zaro()]
#' @param path character. Path to the array within the store.
#' @param meta optional pre-fetched ZaroMeta, or the root group meta with
#'   consolidated entries, as for [zaro_read()].
#' @param parallel logical. If `TRUE`, each batch of 4 or more chunks is
#'   fetched and decoded with `future.apply::future_lapply()`, as for
#'   [zaro_read()] (the same caveats about store serialization apply).
#' @param verbose logical. Emit diagnostic messages when reading metadata.
#' @returns A lazy array, see [altarr::altarr()].
#'
#' @export
#' @examples
#' \dontrun{
#' store <- zaro("https://ncsa.osn.xsede.org/Pangeo/pangeo-forge/gpcp-feedstock/gpcp.zarr")
#' x <- zaro_array(store, "precip")
#' dim(x)                       # longitude, latitude, time
#' x[cbind(180, 90, 1:10)]      # one pixel, ten time steps, one fetch
#' altarr::altarr_extract(x, 1:20, 1:20, 100)
#' altarr::altarr_stats(x)
#' }
zaro_array <- function(store, path = "", meta = NULL, parallel = FALSE,
                       verbose = TRUE) {
  if (!requireNamespace("altarr", quietly = TRUE)) {
    stop("zaro_array() requires the altarr package:\n",
         "  remotes::install_github(\"hypertidy/altarr\")", call. = FALSE)
  }
  if (path == ".") path <- ""
  meta <- resolve_array_meta(store, path, meta, verbose)
  spec <- zaro_array_spec(meta)

  altarr::altarr(spec$dim, spec$chunk,
                 zaro_fetch_factory(store, path, meta, spec, parallel),
                 dimnames = spec$dimnames, type = spec$type)
}


# -- internal helpers ---------------------------------------------------------

#' R-side layout of a Zarr array: dims, chunk shape, type, dimnames
#' @noRd
zaro_array_spec <- function(meta) {
  if (any(vapply(meta@codecs, function(c) identical(c$name, "transpose"),
                 logical(1)))) {
    stop("zaro_array() does not support the 'transpose' codec", call. = FALSE)
  }
  order <- meta@raw_meta[["order"]] %||% "C"
  rev_c <- identical(order, "C")
  shape <- meta@shape
  chunk <- meta@chunk_shape
  dn <- meta@dimension_names
  if (rev_c) {
    shape <- rev(shape)
    chunk <- rev(chunk)
    if (!is.null(dn)) dn <- rev(dn)
  }
  dimnames <- NULL
  if (!is.null(dn) && length(dn) == length(shape)) {
    dimnames <- vector("list", length(shape))
    names(dimnames) <- dn
  }
  type <- switch(dtype_info(meta@data_type)$what,
                 logical = "logical", integer = "integer", "double")
  list(dim = as.integer(shape), chunk = as.integer(chunk), rev_c = rev_c,
       dimnames = dimnames, type = type)
}


#' Build the altarr fetch function for a Zarr array
#'
#' A factory, so the closure carries only what it needs (see
#' altarr's contract on saving recipes).
#' @noRd
zaro_fetch_factory <- function(store, path, meta, spec, parallel = FALSE) {
  force(store); force(path); force(meta); force(spec); force(parallel)
  d <- spec$dim
  cs <- spec$chunk
  full <- prod(cs)
  cast <- switch(spec$type, logical = as.logical, integer = as.integer,
                 as.double)
  fill <- suppressWarnings(
    cast(coerce_fill_value(meta@fill_value, meta@data_type))[1L])

  one <- function(cc) {
    st <- cc * cs + 1L
    ext <- as.integer(pmin(cs, d - st + 1L))
    zidx <- if (spec$rev_c) rev(cc) else cc
    raw <- fetch_chunk(store, path, as.integer(zidx), meta)
    if (is.null(raw)) return(rep(fill, prod(ext)))
    v <- cast(decode_chunk(raw, meta))
    clip_chunk(v, cs, ext)
  }

  function(chunks) {
    rows <- lapply(seq_len(nrow(chunks)), function(r) chunks[r, ])
    if (isTRUE(parallel) && length(rows) >= 4L &&
        requireNamespace("future.apply", quietly = TRUE)) {
      future.apply::future_lapply(rows, one)
    } else {
      lapply(rows, one)
    }
  }
}


#' Clip a decoded chunk to its extent at the array edge
#'
#' Zarr edge chunks are usually stored padded to the full chunk shape; some
#' writers store them truncated. `cs` and `ext` are in R dim order.
#' @noRd
clip_chunk <- function(v, cs, ext) {
  n <- prod(ext)
  if (length(v) == n) return(v)
  if (length(v) < prod(cs)) {
    stop("decoded chunk has ", length(v), " values, expected ", prod(cs),
         " (or ", n, " for an edge chunk)", call. = FALSE)
  }
  a <- array(v[seq_len(prod(cs))], cs)
  as.vector(do.call(`[`, c(list(a), lapply(ext, seq_len), list(drop = FALSE))))
}
