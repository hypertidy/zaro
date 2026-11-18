# Read a single chunk by grid index

Fetches and decodes one chunk, identified by its position in the chunk
grid. Returns the decoded values with metadata about where the chunk
sits in the array.

## Usage

``` r
zaro_chunk(
  store,
  path,
  chunk_idx,
  meta = NULL,
  shaped = TRUE,
  verbose = zaro_verbose()
)
```

## Arguments

- store:

  a store object from
  [`zaro()`](https://hypertidy.github.io/zaro/reference/zaro.md)

- path:

  character. Path to the array.

- chunk_idx:

  integer vector. 0-based chunk grid index (e.g. `c(0, 2, 3)` for the
  first time step, third lat chunk, fourth lon chunk).

- meta:

  optional pre-fetched ZaroMeta (or root group meta).

- shaped:

  logical. If TRUE (default), reshape the values to the chunk's actual
  dimensions (handling C-order and edge chunks). If FALSE, return a flat
  vector.

- verbose:

  logical. Emit diagnostic messages. Default
  `getOption("zaro.verbose", TRUE)`.

## Value

A list with components:

- cidx:

  integer vector - the chunk grid index

- values:

  array or vector - decoded chunk data

- start:

  integer vector - 0-based array coordinates of chunk origin

- shape:

  integer vector - actual shape of this chunk (may be smaller than
  chunk_shape for edge chunks)

Returns NULL if the chunk doesn't exist (missing/sparse).

## Examples

``` r
f <- system.file("extdata", "example.zarr", package = "zaro")
store <- zaro(f)
#> [zaro] opening local store: /usr/local/lib/R/site-library/zaro/extdata/example.zarr
ch <- zaro_chunk(store, "temp", c(0, 1, 1))
#> [zaro] found temp/zarr.json (Zarr V3)
#> [zaro]   dtype: float32 | shape: [3, 8, 16] | chunks: [1, 4, 8]
#> [zaro]   codecs: bytes -> gzip
#> [zaro]   dimensions: time, lat, lon
ch$start           # where the chunk sits in the array (0-based)
#> [1] 0 4 8
dim(ch$values)
#> [1] 1 4 8
```
