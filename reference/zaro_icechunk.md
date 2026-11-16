# Open an Icechunk repository as a zaro store

Reads one snapshot of an [Icechunk](https://icechunk.io) repository
through the tabularberg package. Everything zaro does with a store works
on the result:
[`zaro_meta()`](https://hypertidy.github.io/zaro/reference/zaro_meta.md),
[`zaro_read()`](https://hypertidy.github.io/zaro/reference/zaro_read.md),
[`zaro_array()`](https://hypertidy.github.io/zaro/reference/zaro_array.md),
[`zaro_dataset()`](https://hypertidy.github.io/zaro/reference/zaro_dataset.md),
[`zaro_plan()`](https://hypertidy.github.io/zaro/reference/zaro_plan.md).

## Usage

``` r
zaro_icechunk(
  source,
  branch = NULL,
  tag = NULL,
  snapshot = NULL,
  authorize = NULL,
  direct = FALSE,
  verbose = zaro_verbose()
)
```

## Arguments

- source:

  an `icechunk_repo` or `icechunk_session` from tabularberg, or a
  repository location for `tabularberg::ic_repo_open()` (a local path,
  an `http(s)://` URL, or an `icechunk_storage`).

- branch, tag, snapshot:

  the version to read (one of them; default branch `"main"`). Ignored
  when `source` is a session.

- authorize:

  virtual chunk containers to authorise, passed to
  `tabularberg::ic_repo_open()` when `source` is a location.

- direct:

  logical. Fetch virtual chunks with zaro's own fetcher instead of
  through Icechunk (see Details).

- verbose:

  logical. Emit diagnostic messages.

## Value

An `IcechunkStore`.

## Details

The session is read-only and pinned to one snapshot (its id is in
`store@snapshot`), so lazy arrays made from it keep reading the same
version after the branch moves on. Pick the version with `branch`, `tag`
or `snapshot` (default branch `"main"`).

Chunks are read through Icechunk by default, which applies the
repository's container authorisation and checksums. With `direct = TRUE`
zaro fetches virtual chunks itself, concurrently and with adjacent byte
ranges merged, but only from containers the repository object has
authorised (see `tabularberg::ic_repo_containers()`), and without
Icechunk's etag / last-modified checks. Native and inline chunks always
come from Icechunk.

## Examples

``` r
if (FALSE) { # \dontrun{
ex <- tabularberg::ic_example_repo()
store <- zaro_icechunk(ex$path, tag = "v1", authorize = ex$authorize)
x <- zaro_array(store, "from_bin")
x[1:4, 1:4]
zaro_dataset(store)
} # }
```
