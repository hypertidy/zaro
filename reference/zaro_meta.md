# Get metadata for a Zarr node

Reads and parses the metadata for an array or group at the given path.
Automatically detects Zarr V2 (`.zarray`/`.zattrs`/`.zmetadata`) vs V3
(`zarr.json`) format. For cloud stores, consolidated metadata
(`.zmetadata` for V2, consolidated `zarr.json` for V3) is preferred to
minimise the number of requests.

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
