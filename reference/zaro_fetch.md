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
`"stats"`: HTTP `requests`, `bytes` received and `seconds`.

## Examples

``` r
if (FALSE) { # \dontrun{
p <- zaro_plan(store, "temp", start = c(0, 0, 0, 0), count = c(1, 1, NA, NA))
raw <- zaro_fetch(store, p)
attr(raw, "stats")
} # }
```
