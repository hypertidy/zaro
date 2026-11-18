# List contents of a Zarr store

List contents of a Zarr store

## Usage

``` r
zaro_list(store, path = "", ...)
```

## Arguments

- store:

  a store object from
  [`zaro()`](https://hypertidy.github.io/zaro/reference/zaro.md)

- path:

  character. Path prefix within the store (default root).

- ...:

  arguments for methods (specifically 'recursive = FALSE' is default for
  local and Arrow stores)

## Value

character vector of keys (relative paths).

## Examples

``` r
f <- system.file("extdata", "example.zarr", package = "zaro")
store <- zaro(f)
#> [zaro] opening local store: /usr/local/lib/R/site-library/zaro/extdata/example.zarr
zaro_list(store)
#> [1] "detail"    "lat"       "lon"       "temp"      "time"      "zarr.json"
zaro_list(store, "temp")
#> [1] "temp/c"         "temp/zarr.json"
```
