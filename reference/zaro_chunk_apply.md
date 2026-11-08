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
if (FALSE) { # \dontrun{
# per-chunk mean, memory-efficient
means <- zaro_chunk_apply(store, "temperature", function(chunk) {
  mean(chunk$values, na.rm = TRUE)
})

# first time step only, all spatial chunks
means <- zaro_chunk_apply(store, "temperature", function(chunk) {
  c(mean = mean(chunk$values, na.rm = TRUE),
    n_valid = sum(is.finite(chunk$values)))
}, ranges = list(0, NULL, NULL))
} # }
```
