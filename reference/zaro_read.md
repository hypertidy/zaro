# Read data from a Zarr array

Reads a hyperslab from a Zarr array, fetching and decoding the necessary
chunks and assembling them into an R array. Supports Zarr V2 and V3,
including sharded V3 arrays, and every store
[`zaro()`](https://hypertidy.github.io/zaro/reference/zaro.md) opens.
The chunks are planned with
[`zaro_plan()`](https://hypertidy.github.io/zaro/reference/zaro_plan.md)
and fetched together with
[`zaro_fetch()`](https://hypertidy.github.io/zaro/reference/zaro_fetch.md).

## Usage

``` r
zaro_read(
  store,
  path = "",
  start = NULL,
  count = NULL,
  meta = NULL,
  parallel = FALSE,
  verbose = zaro_verbose(),
  assemble = TRUE
)
```

## Arguments

- store:

  a store object from
  [`zaro()`](https://hypertidy.github.io/zaro/reference/zaro.md), or a
  [`zaro_dataset()`](https://hypertidy.github.io/zaro/reference/zaro_dataset.md)
  (then `path` is an array's name in the dataset)

- path:

  character. Path to the array within the store.

- start:

  integer vector. 0-based start indices for each dimension. Defaults to
  the origin.

- count:

  integer vector. Number of elements to read along each dimension. Use
  `NA` for any axis to read the full extent from `start`. Defaults to
  the full extent.

- meta:

  optional pre-fetched ZaroMeta object. If NULL (default), metadata is
  read from the store. Can also be the root group metadata from
  `zaro_meta(store)` - the array will be looked up by path in the
  consolidated entries.

- parallel:

  logical. If `TRUE`, decode chunks in parallel using
  [`future.apply::future_lapply()`](https://future.apply.futureverse.org/reference/future_lapply.html).
  Fetching is always one pooled
  [`zaro_fetch()`](https://hypertidy.github.io/zaro/reference/zaro_fetch.md)
  (concurrent for HTTP), so this helps only when decoding is the cost.
  Requires the `future.apply` package and a
  [`future::plan()`](https://future.futureverse.org/reference/plan.html),
  e.g. `future::plan(future.mirai::mirai_multisession)`. Skipped for
  reads of fewer than 4 chunks. Default `FALSE`.

- verbose:

  logical. Emit diagnostic messages. Default
  `getOption("zaro.verbose", TRUE)`.

- assemble:

  logical. If `TRUE` (the default) return the assembled array, else the
  list of decoded chunks.

## Value

An R array with dimensions matching `count`, in Zarr dimension order and
named by the array's dimension names (the transpose of
[`zaro_array()`](https://hypertidy.github.io/zaro/reference/zaro_array.md)
for C-order arrays). String arrays read as character.

## Examples

``` r
f <- system.file("extdata", "example.zarr", package = "zaro")
store <- zaro(f)
#> [zaro] opening local store: /usr/local/lib/R/site-library/zaro/extdata/example.zarr
## the first time step, every lat and lon (start is 0-based)
x <- zaro_read(store, "temp", start = c(0, 0, 0), count = c(1, NA, NA))
#> [zaro] found temp/zarr.json (Zarr V3)
#> [zaro]   dtype: float32 | shape: [3, 8, 16] | chunks: [1, 4, 8]
#> [zaro]   codecs: bytes -> gzip
#> [zaro]   dimensions: time, lat, lon
#> [zaro] reading 4 chunk(s) for path 'temp' (V3)
dim(x)
#> time  lat  lon 
#>    1    8   16 
## a window of a sharded array
zaro_read(store, "detail/elev", start = c(2, 2), count = c(3, 4))
#> [zaro] found detail/elev/zarr.json (Zarr V3)
#> [zaro]   dtype: int16 | shape: [16, 16] | chunks: [4, 4]
#> [zaro]   codecs: bytes -> gzip
#> [zaro]   dimensions: y, x
#> [zaro] reading 4 chunk(s) for path 'detail/elev' (V3)
#>      [,1] [,2] [,3] [,4]
#> [1,]  202  203  204  205
#> [2,]  302  303  304  305
#> [3,]  402  403  404  405
```
