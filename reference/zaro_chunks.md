# Read multiple chunks by index range

Fetches and decodes a set of chunks identified by ranges along each
dimension of the chunk grid, one at a time or in parallel. For batched,
pooled reads use
[`zaro_plan()`](https://hypertidy.github.io/zaro/reference/zaro_plan.md)
and
[`zaro_fetch()`](https://hypertidy.github.io/zaro/reference/zaro_fetch.md),
or
[`zaro_read()`](https://hypertidy.github.io/zaro/reference/zaro_read.md).

## Usage

``` r
zaro_chunks(
  store,
  path,
  ranges = NULL,
  meta = NULL,
  shaped = TRUE,
  parallel = FALSE,
  verbose = zaro_verbose()
)
```

## Arguments

- store:

  a store object from
  [`zaro()`](https://hypertidy.github.io/zaro/reference/zaro.md)

- path:

  character. Path to the array.

- ranges:

  a list of integer vectors, one per dimension, giving the 0-based chunk
  indices to read. Use `NULL` for "all chunks along this dimension".
  E.g. `list(0, NULL, 2:4)` reads chunk 0 along dim 1, all chunks along
  dim 2, chunks 2-4 along dim 3.

- meta:

  optional pre-fetched ZaroMeta (or root group meta).

- shaped:

  logical. If TRUE, reshape each chunk's values.

- parallel:

  logical. Use future.apply for parallel fetch+decode.

- verbose:

  logical. Emit diagnostic messages. Default
  `getOption("zaro.verbose", TRUE)`.

## Value

A list of chunk results (as from
[`zaro_chunk()`](https://hypertidy.github.io/zaro/reference/zaro_chunk.md)).
NULL entries indicate missing chunks.

## Examples

``` r
f <- system.file("extdata", "example.zarr", package = "zaro")
store <- zaro(f)
#> [zaro] opening local store: /usr/local/lib/R/site-library/zaro/extdata/example.zarr
## every chunk of the first time step
chs <- zaro_chunks(store, "temp", ranges = list(0, NULL, NULL))
#> [zaro] found temp/zarr.json (Zarr V3)
#> [zaro]   dtype: float32 | shape: [3, 8, 16] | chunks: [1, 4, 8]
#> [zaro]   codecs: bytes -> gzip
#> [zaro]   dimensions: time, lat, lon
#> [zaro] reading 4 chunk(s) for path 'temp'
length(chs)
#> [1] 4
t(sapply(chs, `[[`, "start"))
#>      [,1] [,2] [,3]
#> [1,]    0    0    0
#> [2,]    0    4    0
#> [3,]    0    0    8
#> [4,]    0    4    8
```
