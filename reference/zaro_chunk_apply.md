# Apply a function over chunks

Iterates over chunks and applies a function to each, accumulating
results. Chunks are fetched and decoded one at a time (or in parallel)
so memory usage is proportional to one chunk, not the full array.

## Usage

``` r
zaro_chunk_apply(
  store,
  path,
  fun,
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

- fun:

  function. Called with a single chunk result list (as from
  [`zaro_chunk()`](https://hypertidy.github.io/zaro/reference/zaro_chunk.md)).
  NULL chunks (missing) are skipped.

- ranges:

  optional range specification (as in
  [`zaro_chunks()`](https://hypertidy.github.io/zaro/reference/zaro_chunks.md)).

- meta:

  optional pre-fetched ZaroMeta (or root group meta).

- shaped:

  logical. If TRUE, reshape each chunk's values.

- parallel:

  logical. Use future.apply for parallel execution.

- verbose:

  logical. Emit diagnostic messages. Default
  `getOption("zaro.verbose", TRUE)`.

## Value

A list of results from `fun`, one per non-NULL chunk.

## Examples

``` r
f <- system.file("extdata", "example.zarr", package = "zaro")
store <- zaro(f)
#> [zaro] opening local store: /usr/local/lib/R/site-library/zaro/extdata/example.zarr
## per-chunk mean, one chunk in memory at a time
means <- zaro_chunk_apply(store, "temp", function(chunk) {
  mean(chunk$values, na.rm = TRUE)
})
#> [zaro] found temp/zarr.json (Zarr V3)
#> [zaro]   dtype: float32 | shape: [3, 8, 16] | chunks: [1, 4, 8]
#> [zaro]   codecs: bytes -> gzip
#> [zaro]   dimensions: time, lat, lon
#> [zaro] reading 12 chunk(s) for path 'temp'
unlist(means)
#>  [1] 15.08707 16.10000 17.10000 14.10000 15.10000 16.10000 15.90000 16.90000
#>  [9] 17.90000 14.90000 15.90000 16.90000

## first time step only, all spatial chunks
zaro_chunk_apply(store, "temp", function(chunk) {
  c(mean = mean(chunk$values, na.rm = TRUE),
    n_valid = sum(is.finite(chunk$values)))
}, ranges = list(0, NULL, NULL))
#> [zaro] found temp/zarr.json (Zarr V3)
#> [zaro]   dtype: float32 | shape: [3, 8, 16] | chunks: [1, 4, 8]
#> [zaro]   codecs: bytes -> gzip
#> [zaro]   dimensions: time, lat, lon
#> [zaro] reading 4 chunk(s) for path 'temp'
#> [[1]]
#>     mean  n_valid 
#> 15.08707 29.00000 
#> 
#> [[2]]
#>    mean n_valid 
#>    14.1    32.0 
#> 
#> [[3]]
#>    mean n_valid 
#>    15.9    32.0 
#> 
#> [[4]]
#>    mean n_valid 
#>    14.9    32.0 
#> 
```
