# Get metadata for a Zarr node

Reads and parses the metadata for an array or group at the given path.
Automatically detects Zarr V2 (`.zarray`/`.zattrs`/`.zmetadata`) vs V3
(`zarr.json`) format. For cloud stores, consolidated metadata
(`.zmetadata` for V2, consolidated `zarr.json` for V3) is preferred to
minimize the number of requests.

## Usage

``` r
zaro_meta(store, path = "", consolidated = TRUE, verbose = zaro_verbose())
```

## Arguments

- store:

  a store object from
  [`zaro()`](https://hypertidy.github.io/zaro/reference/zaro.md)

- path:

  character. Path to the array or group within the store. Use `""` or
  `"/"` for the root group.

- consolidated:

  logical. If `TRUE`, attempt to read consolidated metadata first
  (default).

- verbose:

  logical. Emit diagnostic messages. Default
  `getOption("zaro.verbose", TRUE)`.

## Value

A ZaroMeta object. For root group requests with consolidated metadata,
the individual array metadata are attached as `attr(, "consolidated")`.

## Examples

``` r
f <- system.file("extdata", "example.zarr", package = "zaro")
store <- zaro(f)
#> [zaro] opening local store: /usr/local/lib/R/site-library/zaro/extdata/example.zarr
zaro_meta(store)                  # the root group
#> [zaro] found zarr.json at root (Zarr V3)
#> [zaro] reading consolidated metadata (6 entries)
#> <ZaroMeta> [group]
#>  variables: 
#>     detail           []
#>     lat             [8]
#>     lon            [16]
#>     temp       [3,8,16]
#>     time            [3]
#>     detail/elev [16,16]
#> 
#>   attrs:  1
zaro_meta(store, "temp")          # an array
#> [zaro] found temp/zarr.json (Zarr V3)
#> [zaro]   dtype: float32 | shape: [3, 8, 16] | chunks: [1, 4, 8]
#> [zaro]   codecs: bytes -> gzip
#> [zaro]   dimensions: time, lat, lon
#> <ZaroMeta> [array]
#>   dtype:  float32
#>   shape:  [3 x 8 x 16]
#>   chunks: [1 x 4 x 8]
#>   dims:   time, lat, lon
#>   codecs: bytes -> gzip
#>   fill:   NaN
#>   size:   1.5 KB
#>   attrs:  2
zaro_meta(store, "detail/elev")   # a sharded array in a nested group
#> [zaro] found detail/elev/zarr.json (Zarr V3)
#> [zaro]   dtype: int16 | shape: [16, 16] | chunks: [4, 4]
#> [zaro]   codecs: bytes -> gzip
#> [zaro]   dimensions: y, x
#> <ZaroMeta> [array]
#>   dtype:  int16
#>   shape:  [16 x 16]
#>   chunks: [4 x 4]
#>   shards: [8 x 8] (index at end)
#>   dims:   y, x
#>   codecs: bytes -> gzip
#>   fill:   -1
#>   size:   512.0 B
#>   attrs:  1
```
