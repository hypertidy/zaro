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
