# The groups of a store

Lists every group of a Zarr store, the root included, with the number of
arrays directly in each, so a nested product can be opened one group at
a time with `zaro_dataset(group = )`. Uses consolidated metadata when
the root has it (one request); otherwise lists the store, which needs a
store that can be listed (not plain HTTP).

## Usage

``` r
zaro_groups(source, verbose = zaro_verbose(), ...)
```

## Arguments

- source:

  a store from
  [`zaro()`](https://hypertidy.github.io/zaro/reference/zaro.md), or a
  character source passed to
  [`zaro()`](https://hypertidy.github.io/zaro/reference/zaro.md) along
  with `...`.

- verbose:

  logical. Emit diagnostic messages. Default
  `getOption("zaro.verbose", TRUE)`.

- ...:

  passed to
  [`zaro()`](https://hypertidy.github.io/zaro/reference/zaro.md) when
  `source` is character.

## Value

A data frame with columns `group` (path, `""` for the root) and `arrays`
(number of arrays directly in the group), sorted by path.

## Examples

``` r
if (FALSE) { # \dontrun{
eopf_url <- paste0(
  "https://s3.explorer.eopf.copernicus.eu/esa-zarr-sentinel-explorer-fra/",
  "tests-output/sentinel-2-l2a/",
  "S2C_MSIL2A_20260120T084301_N0511_R064_T36UXA_20260120T122910.zarr")
zaro_groups(eopf_url)
} # }
```
