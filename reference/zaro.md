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

## Usage

``` r
zaro(source, verbose = zaro_verbose(), ..., validate = FALSE)
```

## Arguments

- source:

  character. Path, URI, or reference store locator.

- verbose:

  logical. Emit progress and diagnostic messages. Default
  `getOption("zaro.verbose", TRUE)`; set `options(zaro.verbose = FALSE)`
  to silence zaro everywhere.

- ...:

  store options: for `s3://` and `gs://`, arguments for the Arrow
  filesystem constructors (e.g. `anonymous = TRUE`, `region`,
  `endpoint_override`), also used for curl-pool access; for
  `byterefs://` and `flatrefs://`, `target` (the path remap to apply).

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
if (FALSE) { # \dontrun{
# local store
store <- zaro("/data/my_dataset.zarr")

# S3 store (anonymous)
store <- zaro("s3://mur-sst/zarr-v1", anonymous = TRUE)

# GCS - known public bucket, anonymous auto-detected
store <- zaro(paste0("gs://cmip6/CMIP6/ScenarioMIP/NOAA-GFDL/GFDL-ESM4/",
                     "ssp585/r1i1p1f1/Omon/zos/gn/v20180701"))

# VirtualiZarr Parquet reference
store <- zaro("virtualizarr://https://example.com/dataset.parq")

# byterefs: a SQLite table, or the flat Parquet export
store <- zaro("byterefs:///path/to/store.sqlite")
store <- zaro("flatrefs://https://example.com/store", target = "public")

# then explore
zaro_list(store)
zaro_meta(store, "analysed_sst")
data <- zaro_read(store, "analysed_sst",
                  start = c(0, 0, 0), count = c(1, 100, 100))
} # }
```
