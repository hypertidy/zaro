# Open a Zarr array as a lazy R array

Returns an ordinary R array (a double, integer or logical vector with a
`dim` attribute and no class) whose values are read from the store chunk
by chunk, only when R asks for them. It is an ALTREP object built by
[`altarr::altarr()`](https://hypertidy.github.io/altarr/reference/altarr.html),
with zaro's chunk fetch and codec pipeline behind it. Nothing is read
when the array is created beyond the metadata.

## Usage

``` r
zaro_array(
  store,
  path = "",
  meta = NULL,
  parallel = FALSE,
  verbose = zaro_verbose(),
  cf = FALSE
)
```

## Arguments

- store:

  a store object from
  [`zaro()`](https://hypertidy.github.io/zaro/reference/zaro.md), or a
  [`zaro_dataset()`](https://hypertidy.github.io/zaro/reference/zaro_dataset.md)
  (whose metadata is then used, no request made).

- path:

  character. Path to the array within the store.

- meta:

  optional pre-fetched ZaroMeta, or the root group meta with
  consolidated entries, as for
  [`zaro_read()`](https://hypertidy.github.io/zaro/reference/zaro_read.md).

- parallel:

  logical. If `TRUE`, each batch of 4 or more chunks is fetched and
  decoded with
  [`future.apply::future_lapply()`](https://future.apply.futureverse.org/reference/future_lapply.html),
  as for
  [`zaro_read()`](https://hypertidy.github.io/zaro/reference/zaro_read.md)
  (the same caveats about store serialization apply).

- verbose:

  logical. Emit diagnostic messages when reading metadata. Default
  `getOption("zaro.verbose", TRUE)`.

- cf:

  logical. Apply CF decoding (masking and unpacking, see below). Default
  `FALSE`.

## Value

A lazy array, see
[`altarr::altarr()`](https://hypertidy.github.io/altarr/reference/altarr.html).

## Details

Subsetting with `x[i]`, `x[cbind(i, j, k)]` and
`altarr::altarr_extract(x, i, j, k)` plans the chunks it needs and
fetches them in one batch; [`sum()`](https://rdrr.io/r/base/sum.html),
[`mean()`](https://rdrr.io/r/base/mean.html),
[`min()`](https://rdrr.io/r/base/Extremes.html) and
[`max()`](https://rdrr.io/r/base/Extremes.html) stream the chunk grid.
See
[`?altarr::altarr_contract`](https://hypertidy.github.io/altarr/reference/altarr_contract.html)
for what base R does with a lazy array, and
[`altarr::altarr_stats()`](https://hypertidy.github.io/altarr/reference/altarr_stats.html)
to count fetches.

## Dimension order

A C-order Zarr array of shape `(s1, ..., sn)` (all Zarr V3 arrays, and
V2 arrays with `"order": "C"`) is returned with R `dim`
`c(sn, ..., s1)`, and the dimension names reversed to match. This is the
order the bytes are stored in, so a chunk is used as decoded. For a
typical `(time, lat, lon)` array, `x[lon, lat, time]` is the R
subscript. V2 arrays with `"order": "F"` keep their shape. This is the
transpose of what
[`zaro_read()`](https://hypertidy.github.io/zaro/reference/zaro_read.md)
returns.

## Missing chunks and types

Chunks absent from the store read as the array's `fill_value`. Integer
types up to 32 bits give an integer array (`uint32` and 64-bit integers
give double, as in
[`zaro_read()`](https://hypertidy.github.io/zaro/reference/zaro_read.md)),
floats give double, `bool` gives logical. No CF packing (`scale_factor`,
`add_offset`, `_FillValue`) is applied unless `cf = TRUE`. String arrays
are refused (a lazy array is numeric or logical); read them with
[`zaro_read()`](https://hypertidy.github.io/zaro/reference/zaro_read.md).

## CF decoding

With `cf = TRUE`, values equal to the array's `fill_value` or to the
`_FillValue` or `missing_value` attributes (and NaN) read as `NA`, then
`scale_factor` and `add_offset` are applied. Missing chunks read as
`NA`. A packed array reads as double; an unpacked one keeps its type.
This is what xarray's default decoding and GDAL's nodata plus unscaling
give.

## Saving

[`saveRDS()`](https://rdrr.io/r/base/readRDS.html) on the result writes
the recipe (shape, chunk shape and the fetch function), not the values.
Stores backed by Arrow or GDAL hold external pointers, so a saved array
does not read in a new R session; re-open it with `zaro_array()`.

## Examples

``` r
if (FALSE) { # \dontrun{
store <- zaro("https://ncsa.osn.xsede.org/Pangeo/pangeo-forge/gpcp-feedstock/gpcp.zarr")
x <- zaro_array(store, "precip")
dim(x)                       # longitude, latitude, time
x[cbind(180, 90, 1:10)]      # one pixel, ten time steps, one fetch
altarr::altarr_extract(x, 1:20, 1:20, 100)
altarr::altarr_stats(x)
} # }
```
