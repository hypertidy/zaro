# Plan a read: the byte references a selection needs, without reading them

Works out which chunks of an array a hyperslab touches and where each
chunk's bytes are: a whole object in a native store, a byte range inside
a NetCDF/HDF5/TIFF file for a virtual store (kerchunk, VirtualiZarr,
byterefs), a byte range inside a shard object for a sharded array,
inline bytes, or absent (reads as `fill_value`). No chunk data is read.
For a native unsharded store no request is made at all; a sharded array
reads the index of each shard it touches, and a virtual store may fetch
the manifest or row groups it needs (cached in the store).

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
f <- system.file("extdata", "example.zarr", package = "zaro")
store <- zaro(f)
#> [zaro] opening local store: /usr/local/lib/R/site-library/zaro/extdata/example.zarr
p <- zaro_plan(store, "temp", start = c(0, 0, 0), count = c(1, NA, NA))
#> [zaro] found temp/zarr.json (Zarr V3)
#> [zaro]   dtype: float32 | shape: [3, 8, 16] | chunks: [1, 4, 8]
#> [zaro]   codecs: bytes -> gzip
#> [zaro]   dimensions: time, lat, lon
#> [zaro] planned 4 chunk(s) for path 'temp'
p                          # one row per chunk, whole local files
#> <zaro_plan> 'temp': 4 chunk(s), 4 request(s)
#>   transport: local 4
#>   bytes:     4 whole object(s) of unknown size
#>           key transport offset size                                 path
#>  temp/c/0/0/0     local      0   NA ...extdata/example.zarr/temp/c/0/0/0
#>  temp/c/0/1/0     local      0   NA ...extdata/example.zarr/temp/c/0/1/0
#>  temp/c/0/0/1     local      0   NA ...extdata/example.zarr/temp/c/0/0/1
#>  temp/c/0/1/1     local      0   NA ...extdata/example.zarr/temp/c/0/1/1
## a sharded array: byte ranges within each shard
zaro_plan(store, "detail/elev", start = c(0, 0), count = c(4, 8))
#> [zaro] found detail/elev/zarr.json (Zarr V3)
#> [zaro]   dtype: int16 | shape: [16, 16] | chunks: [4, 4]
#> [zaro]   codecs: bytes -> gzip
#> [zaro]   dimensions: y, x
#> [zaro] planned 2 chunk(s) for path 'detail/elev'
#> <zaro_plan> 'detail/elev': 2 chunk(s), 2 request(s)
#>   transport: local 2
#>   bytes:     103.0 B
#>                key transport offset size                                 path
#>  detail/elev/c/0/0     local      0   51 ...ta/example.zarr/detail/elev/c/0/0
#>  detail/elev/c/0/0     local    104   52 ...ta/example.zarr/detail/elev/c/0/0
if (FALSE) { # interactive()
## a virtual store: byte ranges in NetCDF files on a THREDDS server
## (network access)
vstore <- zaro(paste0("virtualizarr://https://raw.githubusercontent.com/",
  "mdsumner/virtualized/refs/heads/main/remote/ocean_temp_2023.parq"))
p <- zaro_plan(vstore, "temp", start = c(0, 0, 0, 0), count = c(1, 1, NA, NA))
p                          # 60 byte ranges on one host, ~4 MB
table(p$transport)
}
```
