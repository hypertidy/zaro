# Fetch the bytes of a plan

Executes a
[`zaro_plan()`](https://hypertidy.github.io/zaro/reference/zaro_plan.md):
returns the raw (still encoded) bytes of every chunk, in plan order,
`NULL` for chunks that are absent (they read as `fill_value`). HTTP
references are fetched concurrently through one curl connection pool
with a per-host cap, adjacent byte ranges are merged into single
requests, and failures are retried. S3 and GCS objects join the same
pool as HTTPS when their access is known: anonymous
(`zaro(..., anonymous = TRUE)`, a known public bucket,
`AWS_NO_SIGN_REQUEST=YES` or `options(zaro.cloud_anonymous = TRUE)`), or
S3 signed with `AWS_ACCESS_KEY_ID` / `AWS_SECRET_ACCESS_KEY` (and
`AWS_SESSION_TOKEN`) from the environment. Other credentials (profiles,
instance roles, signed GCS) are read through Arrow or GDAL one object at
a time.
[`zaro_read()`](https://hypertidy.github.io/zaro/reference/zaro_read.md)
and
[`zaro_array()`](https://hypertidy.github.io/zaro/reference/zaro_array.md)
read through this, so they get the same batching.

## Usage

``` r
zaro_fetch(
  store,
  plan,
  max_con = getOption("zaro.max_con", 8L),
  merge_gap = getOption("zaro.merge_gap", 0),
  retries = 2L,
  verbose = zaro_verbose()
)
```

## Arguments

- store:

  the store the plan was made from.

- plan:

  a `zaro_plan` (or any refs table with the same columns).

- max_con:

  integer. Maximum simultaneous HTTP connections, per host and in total.
  Default `getOption("zaro.max_con", 8L)`.

- merge_gap:

  numeric. Merge byte ranges on the same URL separated by at most this
  many bytes (the gap is fetched and discarded). Default
  `getOption("zaro.merge_gap", 0)`: merge only touching ranges.

- retries:

  integer. Retries for failed HTTP requests.

- verbose:

  logical. Emit diagnostic messages. Default
  `getOption("zaro.verbose", TRUE)`.

## Value

A list of raw vectors (or `NULL`), one per plan row, with attribute
`"stats"`: HTTP `requests`, `bytes` received, new `connections` opened
and `seconds`.

## Details

Connections are kept open between calls, so a loop that reads a chunk at
a time reuses them rather than opening one per chunk. Set
`options(zaro.http_keepalive = FALSE)` for a fresh pool on every call.

## Examples

``` r
f <- system.file("extdata", "example.zarr", package = "zaro")
store <- zaro(f)
#> [zaro] opening local store: /usr/local/lib/R/site-library/zaro/extdata/example.zarr
p <- zaro_plan(store, "temp", start = c(0, 0, 0), count = c(1, NA, NA))
#> [zaro] found temp/zarr.json (Zarr V3)
#> [zaro]   dtype: float32 | shape: [3, 8, 16] | chunks: [1, 4, 8]
#> [zaro]   codecs: bytes -> gzip
#> [zaro]   dimensions: time, lat, lon
#> [zaro] planned 4 chunk(s) for path 'temp'
raw <- zaro_fetch(store, p)
lengths(raw)                 # encoded (gzip) bytes per chunk
#> [1] 105  97 100  98
attr(raw, "stats")
#> $requests
#> [1] 0
#> 
#> $bytes
#> [1] 0
#> 
#> $connections
#> [1] 0
#> 
#> $seconds
#> [1] 0
#> 
```
