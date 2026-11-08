# Open a Zarr store

Connects to a Zarr store at the given path or URI. Dispatches to the
appropriate backend based on the URI scheme:

- Local paths: Arrow LocalFileSystem

- `s3://`: Arrow S3FileSystem (with automatic region detection)

- `gs://`: Arrow GcsFileSystem

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
  to silence zaro everywhere. Suppress with
  [`suppressMessages()`](https://rdrr.io/r/base/message.html).

- ...:

  additional arguments passed to Arrow filesystem constructors (e.g.
  `anonymous = TRUE`, `endpoint_override`, `region`).

- validate:

  should the store be checked for valid metadata, `FALSE` is default

## Value

A store object (internal class) for use with other zaro functions.

## Details

For S3 stores, if Arrow returns a 301 redirect (wrong region), zaro will
fall through to gdalraster's `/vsis3/` which handles region detection
automatically.

Known public buckets (e.g. cmip6, aodn-cloud-optimised) are accessed
anonymously by default. For other public stores, pass
`anonymous = TRUE`.

Validation of the store may be done by setting `validate = TRUE`, but
this is otherwise avoided at open time by default (`source` is checked
for being character, non- empty string).

## Examples

``` r
if (FALSE) { # \dontrun{
# local store
store <- zaro("/data/my_dataset.zarr")

# S3 store (anonymous)
store <- zaro("s3://mur-sst/zarr-v1", anonymous = TRUE)

# GCS - known public bucket, anonymous auto-detected
store <- zaro("gs://cmip6/CMIP6/ScenarioMIP/NOAA-GFDL/GFDL-ESM4/ssp585/r1i1p1f1/Omon/zos/gn/v20180701")

# VirtualiZarr Parquet reference
store <- zaro("virtualizarr://https://example.com/dataset.parq")

# then explore
zaro_list(store)
zaro_meta(store, "analysed_sst")
data <- zaro_read(store, "analysed_sst",
                  start = c(0, 0, 0), count = c(1, 100, 100))
} # }
```
