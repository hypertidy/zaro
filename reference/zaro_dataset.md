# Open a store as a dataset: every array, its dimensions, no data read

Collects the metadata of every array in one group of a Zarr store (the
root by default) and reports each array's dimension names and sizes in R
order (the order
[`zaro_array()`](https://hypertidy.github.io/zaro/reference/zaro_array.md)
returns). Consolidated metadata (`.zmetadata` or a consolidated
`zarr.json`) is used when present, which is one request for the whole
store. Otherwise the store is listed and each child is probed for array
metadata; pass `arrays` to skip the listing (useful over plain HTTP,
which can't list).

## Usage

``` r
zaro_dataset(source, group = "", arrays = NULL, verbose = zaro_verbose(), ...)
```

## Arguments

- source:

  a store from
  [`zaro()`](https://hypertidy.github.io/zaro/reference/zaro.md), or a
  character source passed to
  [`zaro()`](https://hypertidy.github.io/zaro/reference/zaro.md) along
  with `...`.

- group:

  path of the group to open, e.g. `"measurements/reflectance/r10m"`.
  Default `""`, the root.

- arrays:

  optional character vector of array names (within `group`) to include.
  With consolidated metadata it filters; without, it replaces the
  listing.

- verbose:

  logical. Emit diagnostic messages. Default
  `getOption("zaro.verbose", TRUE)`.

- ...:

  passed to
  [`zaro()`](https://hypertidy.github.io/zaro/reference/zaro.md) when
  `source` is character.

## Value

A `zaro_dataset`: a list with `store`, `group` (the group's path),
`attributes` (the group's attributes), `arrays` (named list of array
metadata), `paths` (named character vector: each array's path in the
store), `dims` (named list of R-order dimension names per array) and
`shape` (named list of R-order sizes per array). Arrays are named within
the group;
[`zaro_array()`](https://hypertidy.github.io/zaro/reference/zaro_array.md),
[`zaro_read()`](https://hypertidy.github.io/zaro/reference/zaro_read.md)
and
[`zaro_plan()`](https://hypertidy.github.io/zaro/reference/zaro_plan.md)
take the dataset with those names.

## Details

No array data is read. Use
[`zaro_array()`](https://hypertidy.github.io/zaro/reference/zaro_array.md)
on the result to get a lazy array, or
[`zaro_read()`](https://hypertidy.github.io/zaro/reference/zaro_read.md)
with `meta = ds$arrays[[name]]`.

Arrays with no dimension names (Zarr V2 without xarray's
`_ARRAY_DIMENSIONS`) get names `"<array>_dim<k>"`, with `k` the 0-based
Zarr axis, so they never collide with another array's dimensions. Only
arrays directly in `group` are included; use
[`zaro_groups()`](https://hypertidy.github.io/zaro/reference/zaro_groups.md)
to see a nested store's groups (e.g. EOPF and GeoZarr products, which
keep their arrays several groups down).

## Examples

``` r
if (FALSE) { # \dontrun{
ds <- zaro_dataset("s3://aodn-cloud-optimised/model_sea_level_anomaly_gridded_realtime.zarr",
                   anonymous = TRUE, region = "ap-southeast-2")
ds
ds$dims$GSLA                       # "LONGITUDE" "LATITUDE" "TIME"
gsla <- zaro_array(ds, "GSLA", cf = TRUE)

## a nested store: list the groups, open one
eopf_url <- paste0(
  "https://s3.explorer.eopf.copernicus.eu/esa-zarr-sentinel-explorer-fra/",
  "tests-output/sentinel-2-l2a/",
  "S2C_MSIL2A_20260120T084301_N0511_R064_T36UXA_20260120T122910.zarr")
eopf <- zaro(eopf_url)
zaro_groups(eopf)
r10m <- zaro_dataset(eopf, group = "measurements/reflectance/r10m")
b04 <- zaro_array(r10m, "b04")
} # }
```
