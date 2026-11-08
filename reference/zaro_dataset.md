# Open a store as a dataset: every array, its dimensions, no data read

Collects the metadata of every array at the root of a Zarr store and
reports each array's dimension names and sizes in R order (the order
[`zaro_array()`](https://hypertidy.github.io/zaro/reference/zaro_array.md)
returns). Consolidated metadata (`.zmetadata` or a consolidated
`zarr.json`) is used when present, which is one request for the whole
store. Otherwise the store is listed and each child is probed for array
metadata; pass `arrays` to skip the listing (useful over plain HTTP,
which can't list).

## Usage

``` r
zaro_dataset(source, arrays = NULL, verbose = zaro_verbose(), ...)
```

## Arguments

- source:

  a store from
  [`zaro()`](https://hypertidy.github.io/zaro/reference/zaro.md), or a
  character source passed to
  [`zaro()`](https://hypertidy.github.io/zaro/reference/zaro.md) along
  with `...`.

- arrays:

  optional character vector of array names to include. With consolidated
  metadata it filters; without, it replaces the listing.

- verbose:

  logical. Emit diagnostic messages. Default
  `getOption("zaro.verbose", TRUE)`.

- ...:

  passed to
  [`zaro()`](https://hypertidy.github.io/zaro/reference/zaro.md) when
  `source` is character.

## Value

A `zaro_dataset`: a list with `store`, `attributes` (root group
attributes), `arrays` (named list of array metadata), `dims` (named list
of R-order dimension names per array) and `shape` (named list of R-order
sizes per array).

## Details

No array data is read. Use
[`zaro_array()`](https://hypertidy.github.io/zaro/reference/zaro_array.md)
on the result to get a lazy array, or
[`zaro_read()`](https://hypertidy.github.io/zaro/reference/zaro_read.md)
with `meta = ds$arrays[[name]]`.

Arrays with no dimension names (Zarr V2 without xarray's
`_ARRAY_DIMENSIONS`) get names `"<array>_dim<k>"`, with `k` the 0-based
Zarr axis, so they never collide with another array's dimensions. Only
arrays directly in the root group are included.

## Examples

``` r
if (FALSE) { # \dontrun{
ds <- zaro_dataset("s3://aodn-cloud-optimised/model_sea_level_anomaly_gridded_realtime.zarr",
                   anonymous = TRUE, region = "ap-southeast-2")
ds
ds$dims$GSLA                       # "LONGITUDE" "LATITUDE" "TIME"
gsla <- zaro_array(ds, "GSLA", cf = TRUE)
} # }
```
