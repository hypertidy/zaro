# Read data from a Zarr array

Reads a hyperslab from a Zarr array, fetching and decoding the necessary
chunks and assembling them into an R array. Supports both Zarr V2 and
V3.

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
  [`zaro()`](https://hypertidy.github.io/zaro/reference/zaro.md)

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

  logical. If `TRUE`, fetch and decode chunks in parallel using
  [`future.apply::future_lapply()`](https://future.apply.futureverse.org/reference/future_lapply.html).
  Requires the `future.apply` package and a
  [`future::plan()`](https://future.futureverse.org/reference/plan.html)
  to be set. Recommended backend:
  `future::plan(future.mirai::mirai_multisession)`. Skipped
  automatically for reads of fewer than 4 chunks. Default FALSE.

- verbose:

  logical. Emit diagnostic messages. Default
  `getOption("zaro.verbose", TRUE)`.

- assemble:

  logical. If `TRUE`return a full array (the default), else the raw list
  of decoded chunks.

## Value

An R array with dimensions matching `count`.
