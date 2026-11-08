# Plan a read: the byte references a selection needs, without reading them

Works out which chunks of an array a hyperslab touches and where each
chunk's bytes are: a whole object in a native store, a byte range inside
a NetCDF/HDF5/TIFF file for a virtual store (kerchunk, VirtualiZarr,
mdim-refs), inline bytes, or absent (reads as `fill_value`). No chunk
data is read. For a native store no request is made at all; a virtual
store may fetch the manifest it needs (cached in the store).

## Usage

``` r
zaro_plan(
  store,
  path = "",
  start = NULL,
  count = NULL,
  meta = NULL,
  chunks = NULL,
  verbose = zaro_verbose()
)
```

## Arguments

- store:

  a store from
  [`zaro()`](https://hypertidy.github.io/zaro/reference/zaro.md), or a
  [`zaro_dataset()`](https://hypertidy.github.io/zaro/reference/zaro_dataset.md).

- path:

  character. Path to the array within the store.

- start, count:

  the selection, as for
  [`zaro_read()`](https://hypertidy.github.io/zaro/reference/zaro_read.md):
  0-based start and element counts in Zarr dimension order, `NA` count
  for the rest of an axis. Default the whole array.

- meta:

  optional array metadata, as for
  [`zaro_read()`](https://hypertidy.github.io/zaro/reference/zaro_read.md).

- chunks:

  optional integer matrix of 0-based chunk grid indices (one row per
  chunk, Zarr dimension order), instead of `start`/`count`.

- verbose:

  logical. Emit diagnostic messages. Default
  `getOption("zaro.verbose", TRUE)`.

## Value

A `zaro_plan` data frame, one row per chunk, in the shape of a byterefs
view (<https://github.com/byterefs/spec>): `dim_0`, ..., `dim_{n-1}`
(the chunk's 0-based grid index, Zarr dimension order), `present`
(`FALSE` for an absent chunk, read as `fill_value`), `path` (where the
bytes are), `offset`, `size` (`NA` for a whole object of unknown size),
`data` (list of raw, for inline chunks), then zaro's own columns: `key`
(the Zarr key) and `transport` (`"store"`, `"http"`, `"s3"`, `"gs"`,
`"vsi"`, `"local"`, `"inline"` or `"absent"`: how the bytes are
fetched). Attributes `array` (the array's path in the store) and `meta`
record what was planned.

## Details

The result is a data frame you can inspect, filter, save or hand to
another tool. Printing it summarises the cost: requests, known bytes and
hosts.

## Examples

``` r
if (FALSE) { # \dontrun{
store <- zaro("virtualizarr://https://raw.githubusercontent.com/mdsumner/virtualized/refs/heads/main/remote/ocean_temp_2023.parq")
p <- zaro_plan(store, "temp", start = c(0, 0, 0, 0), count = c(1, 1, NA, NA))
p                          # 60 byte ranges on one host, ~4 MB
table(p$transport)
} # }
```
