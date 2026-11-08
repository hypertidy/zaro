# Read several whole arrays in one fetch

Plans every array, fetches all the plans with one
[`zaro_fetch()`](https://hypertidy.github.io/zaro/reference/zaro_fetch.md)
(so HTTP chunks of all the arrays share one connection pool and go out
concurrently, and touching byte ranges merge), then decodes each array.
Each result is what `zaro_read(store, path)` returns. Useful for reading
a dataset's coordinate arrays at open, where each array is small and the
cost is the round trips.

## Usage

``` r
zaro_read_arrays(store, paths, metas = NULL, verbose = zaro_verbose())
```

## Arguments

- store:

  a store from
  [`zaro()`](https://hypertidy.github.io/zaro/reference/zaro.md), or a
  [`zaro_dataset()`](https://hypertidy.github.io/zaro/reference/zaro_dataset.md)
  (then `paths` are array names in the dataset).

- paths:

  character vector of array paths (or dataset names).

- metas:

  optional list of array metadata, one per path (taken from the dataset
  when `store` is one, else read).

- verbose:

  logical. Emit diagnostic messages. Default
  `getOption("zaro.verbose", TRUE)`.

## Value

A named list of arrays, one per path, each with `dim` in Zarr order as
from
[`zaro_read()`](https://hypertidy.github.io/zaro/reference/zaro_read.md).
Fetch statistics are in `attr(, "stats")`.

## Examples

``` r
if (FALSE) { # \dontrun{
ds <- zaro_dataset("https://host/store.zarr")
xy <- zaro_read_arrays(ds, c("x", "y"))
attr(xy, "stats")$requests
} # }
```
