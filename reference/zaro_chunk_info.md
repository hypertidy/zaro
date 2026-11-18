# Get chunk grid information for an array

Returns the chunk grid dimensions and related metadata needed for
chunk-indexed operations.

## Usage

``` r
zaro_chunk_info(store, path, meta = NULL, verbose = zaro_verbose())
```

## Arguments

- store:

  a store object from
  [`zaro()`](https://hypertidy.github.io/zaro/reference/zaro.md)

- path:

  character. Path to the array.

- meta:

  optional pre-fetched ZaroMeta (or root group meta).

- verbose:

  logical. Emit diagnostic messages. Default
  `getOption("zaro.verbose", TRUE)`.

## Value

A list with components:

- grid:

  integer vector - number of chunks along each dimension

- chunk_shape:

  integer vector - shape of a full (non-edge) chunk

- array_shape:

  integer vector - shape of the full array

- n_chunks:

  integer - total number of chunks

- dimension_names:

  character vector or NULL

- meta:

  ZaroMeta for the array

## Examples

``` r
f <- system.file("extdata", "example.zarr", package = "zaro")
store <- zaro(f)
#> [zaro] opening local store: /usr/local/lib/R/site-library/zaro/extdata/example.zarr
info <- zaro_chunk_info(store, "temp")
#> [zaro] found temp/zarr.json (Zarr V3)
#> [zaro]   dtype: float32 | shape: [3, 8, 16] | chunks: [1, 4, 8]
#> [zaro]   codecs: bytes -> gzip
#> [zaro]   dimensions: time, lat, lon
info$grid          # chunks along time, lat, lon
#> [1] 3 2 2
info$chunk_shape
#> [1] 1 4 8
```
