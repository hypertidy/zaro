# Open a Zarr store

Connects to a Zarr store at the given path or URI. Dispatches to the
appropriate backend based on the URI scheme:

- Local paths: base R (no arrow or GDAL needed)

- `s3://`, `gs://`: listed and read through Arrow when arrow is
  installed, else through GDAL's `/vsis3/` or `/vsigs/` (gdalraster).
  When access is known (anonymous, or S3 keys in the environment) chunks
  are fetched as HTTPS through the curl pool instead, see
  [`zaro_fetch()`](https://hypertidy.github.io/zaro/reference/zaro_fetch.md)

- `http://`, `https://`: curl (no GDAL); chunk reads are batched through
  one connection pool, see
  [`zaro_fetch()`](https://hypertidy.github.io/zaro/reference/zaro_fetch.md).
  An HTTP store can't be listed, so it needs consolidated metadata (or
  array names, see
  [`zaro_dataset()`](https://hypertidy.github.io/zaro/reference/zaro_dataset.md))

- `/vsi...`: any GDAL virtual file system path, through gdalraster (e.g.
  `/vsicurl/https://...` to keep GDAL's HTTP handling)

- `reference+json://`: Kerchunk JSON reference store

- `reference+parquet://`: Kerchunk Parquet reference store

- `virtualizarr://`: VirtualiZarr Parquet manifest store

- `byterefs://` (or the older `mdimrefs://`): a byterefs SQLite
  byte-reference table (<https://github.com/byterefs/spec>),
  `byterefs/0.1` or the legacy `mdim-refs/0.2`. Source paths are
  remapped for the store's `meta.default_target`; pass
  `target = "public"` for another target, or `target = ""` for the
  verbatim sources

- `flatrefs://`: a byterefs flat refs store, the Parquet export (a
  directory, local or remote, with `arrays.json` and one
  `refs_<array>.parquet` per array, as raad.virtualize publishes). Chunk
  references are read by row group, only those whose dim statistics can
  hold the selection, so a table of tens of millions of rows is never
  read whole. `target` as for `byterefs://`

- `icechunk://`: an Icechunk repository through tabularberg, e.g.
  `icechunk:///path/to/repo?tag=v1`; the query picks `branch`, `tag` or
  `snapshot`. An `icechunk_repo`, `icechunk_session` or
  `icechunk_storage` object works too. See
  [`zaro_icechunk()`](https://hypertidy.github.io/zaro/reference/zaro_icechunk.md)

## Usage

``` r
zaro(source, verbose = zaro_verbose(), ..., validate = FALSE)
```

## Arguments

- source:

  character. Path, URI, or reference store locator; or an Icechunk
  repository, session or storage from tabularberg (see
  [`zaro_icechunk()`](https://hypertidy.github.io/zaro/reference/zaro_icechunk.md)).

- verbose:

  logical. Emit progress and diagnostic messages. Default
  `getOption("zaro.verbose", TRUE)`; set `options(zaro.verbose = FALSE)`
  to silence zaro everywhere.

- ...:

  store options: for `s3://` and `gs://`, arguments for the Arrow
  filesystem constructors (e.g. `anonymous = TRUE`, `region`,
  `endpoint_override`), also used for curl-pool access; for
  `byterefs://` and `flatrefs://`, `target` (the path remap to apply);
  for Icechunk sources, the arguments of
  [`zaro_icechunk()`](https://hypertidy.github.io/zaro/reference/zaro_icechunk.md)
  (`branch`, `tag`, `snapshot`, `authorize`, `direct`).

- validate:

  logical. Read the root metadata at open, warning if that fails.
  Default `FALSE`.

## Value

A store object (internal class) for use with other zaro functions.

## Details

For S3 stores, if Arrow returns a 301 redirect (wrong region), zaro will
fall through to gdalraster's `/vsis3/` which handles region detection
automatically.

Known public buckets (e.g. cmip6, aodn-cloud-optimised) are accessed
anonymously by default. For other public stores, pass
`anonymous = TRUE`.

Opening reads nothing from the store by default (`source` is only
checked to be a non-empty character string); set `validate = TRUE` to
read the root metadata at open.

## Examples

``` r
## a small Zarr V3 store shipped with the package
f <- system.file("extdata", "example.zarr", package = "zaro")
store <- zaro(f)
#> [zaro] opening local store: /usr/local/lib/R/site-library/zaro/extdata/example.zarr
store
#> <ZaroStore> [zaro::LocalStore]
#>   root: /usr/local/lib/R/site-library/zaro/extdata/example.zarr
zaro_list(store)
#> [1] "detail"    "lat"       "lon"       "temp"      "time"      "zarr.json"
zaro_read(store, "temp", start = c(0, 0, 0), count = c(1, 4, 8))
#> [zaro] found temp/zarr.json (Zarr V3)
#> [zaro]   dtype: float32 | shape: [3, 8, 16] | chunks: [1, 4, 8]
#> [zaro]   codecs: bytes -> gzip
#> [zaro]   dimensions: time, lat, lon
#> [zaro] reading 1 chunk(s) for path 'temp' (V3)
#> , , 1
#> 
#>      [,1]   [,2]   [,3]   [,4]
#> [1,]  NaN 14.875 14.625 14.375
#> 
#> , , 2
#> 
#>      [,1]   [,2]   [,3]   [,4]
#> [1,]  NaN 14.975 14.725 14.475
#> 
#> , , 3
#> 
#>      [,1]   [,2]   [,3]   [,4]
#> [1,]  NaN 15.075 14.825 14.575
#> 
#> , , 4
#> 
#>        [,1]   [,2]   [,3]   [,4]
#> [1,] 15.425 15.175 14.925 14.675
#> 
#> , , 5
#> 
#>        [,1]   [,2]   [,3]   [,4]
#> [1,] 15.525 15.275 15.025 14.775
#> 
#> , , 6
#> 
#>        [,1]   [,2]   [,3]   [,4]
#> [1,] 15.625 15.375 15.125 14.875
#> 
#> , , 7
#> 
#>        [,1]   [,2]   [,3]   [,4]
#> [1,] 15.725 15.475 15.225 14.975
#> 
#> , , 8
#> 
#>        [,1]   [,2]   [,3]   [,4]
#> [1,] 15.825 15.575 15.325 15.075
#> 

## other sources (see the "Connecting to stores" article):
## zaro("s3://bucket/store.zarr", anonymous = TRUE)
## zaro("gs://bucket/store.zarr")
## zaro("virtualizarr://https://host/dataset.parq")
## zaro("byterefs:///path/to/store.sqlite")
## zaro("flatrefs://https://host/store", target = "public")
if (FALSE) { # interactive() && requireNamespace("blosc", quietly = TRUE)
## a public store over HTTPS (network access; blosc-compressed chunks)
aodn <- zaro(paste0("https://aodn-cloud-optimised.s3.ap-southeast-2.amazonaws.com/",
                    "model_sea_level_anomaly_gridded_realtime.zarr"))
zaro_meta(aodn, "GSLA")
zaro_read(aodn, "GSLA", start = c(0, 100, 100), count = c(1, 2, 3))
}
```
