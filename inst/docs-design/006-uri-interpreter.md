# URI Interpreter

_May 2026_

## The problem

The Zarr specification defines a storage model (key-value store with
JSON metadata and binary chunks) but says nothing about how stores
are addressed. Every tool in the ecosystem has invented its own URI
conventions, and users encounter all of them when following tutorials,
catalogue links, and paper supplements.

A single dataset might be referenced as:

```
gs://cmip6/CMIP6/CMIP/NOAA-GFDL/GFDL-ESM4/historical/r2i1p1f1/Omon/tos/gn/v20180701/
https://storage.googleapis.com/cmip6/CMIP6/CMIP/NOAA-GFDL/GFDL-ESM4/historical/r2i1p1f1/Omon/tos/gn/v20180701/
https://storage.googleapis.com/cmip6/CMIP6/CMIP/NOAA-GFDL/GFDL-ESM4/historical/r2i1p1f1/Omon/tos/gn/v20180701/::catalog=static/catalog.json::varname=tos
```

All three point to the same bytes. The user shouldn't need to know
which form their tool expects.

## Known URI forms in the wild

### Native cloud protocols

```
s3://bucket/path                          # AWS S3 via Arrow
gs://bucket/path                          # Google Cloud via Arrow
```

Direct, efficient, uses native client libraries with parallel I/O
and resumable transfers.

### HTTPS equivalents of cloud storage

```
https://storage.googleapis.com/bucket/path          # GCS public URL
https://bucket.s3.amazonaws.com/path                # S3 virtual-hosted
https://s3.amazonaws.com/bucket/path                # S3 path-style
https://s3.region.amazonaws.com/bucket/path         # S3 path-style with region
https://bucket.s3.region.amazonaws.com/path         # S3 virtual-hosted with region
```

These work but go through HTTP rather than native protocols.
Translation to `gs://` or `s3://` enables better I/O performance.

### Custom S3-compatible endpoints

```
https://objects.eodc.eu/tenant:bucket/path          # CEPH with tenant
https://play.min.io/bucket/path                     # MinIO
https://s3.wasabisys.com/bucket/path                # Wasabi
```

These can't use Arrow's S3 client (colon in bucket, non-AWS endpoint).
Plain HTTP via `/vsicurl/` or curl works.

### Reference stores

```
reference+json:///path/to/refs.json                 # Kerchunk JSON
reference+parquet:///path/to/refs.parquet            # Kerchunk Parquet (unused?)
virtualizarr://https://example.com/dataset.parq     # VirtualiZarr manifests
virtualizarr:///local/path/dataset.parq             # VirtualiZarr local
```

These are zaro conventions. The `virtualizarr://` prefix is necessary
because the store can't be distinguished from a plain Zarr store
without probing.

### Catalogue viewer URLs

```
https://storage.googleapis.com/bucket/path/::catalog=static/catalog.json::projectionCenterLat=0::varname=tos
https://example.com/dataset.zarr?variable=temp&time=2023-01-01
```

Viewer-specific parameters appended to the store URL. The `::key=value`
convention is from Pangeo's catalogue viewer. Query parameters may
appear on other platforms. Neither is part of the Zarr store address.

### NASA / Earthdata

```
earthaccess://COLLECTION_ID                         # NASA Earthdata
https://opendap.earthdata.nasa.gov/providers/...    # OPeNDAP
https://data.lpdaac.earthdata.nasa.gov/...          # Direct download
```

NASA data often requires authentication via Earthdata Login tokens.
The actual Zarr stores (when they exist) are on S3 behind auth.

### Plain HTTP

```
https://ncsa.osn.xsede.org/Pangeo/pangeo-forge/gpcp-feedstock/gpcp.zarr
https://s3.waw3-1.cloudferro.com/mdl-arco-time-045/arco/.../timeChunked.zarr
```

No translation needed — these go directly to gdalraster `/vsicurl/`.

## Proposed solution: `zaro_uri()`

A normalisation function that takes any of the above forms and returns
something `zaro()` can open directly.

```r
store <- zaro(zaro_uri(url))
```

### Implementation

```r
zaro_uri <- function(url) {
  # 1. Strip viewer/catalogue parameters
  url <- sub("::.*$", "", url)
  url <- sub("\\?.*$", "", url)  # be cautious — some stores use query params

  # 2. Strip trailing slashes
  url <- sub("/+$", "", url)

  # 3. Translate known HTTPS bases to native protocols
  url <- translate_known_base(url)

  # 4. Detect virtual-hosted S3 URLs
  url <- translate_virtual_hosted_s3(url)

  url
}
```

### Translation rules

**GCS HTTPS → gs://**

```r
# https://storage.googleapis.com/bucket/path → gs://bucket/path
if (grepl("^https://storage\\.googleapis\\.com/", url)) {
  url <- sub("^https://storage\\.googleapis\\.com/", "gs://", url)
}
```

**S3 path-style HTTPS → s3://**

```r
# https://s3.amazonaws.com/bucket/path → s3://bucket/path
# https://s3.region.amazonaws.com/bucket/path → s3://bucket/path
if (grepl("^https://s3[.]([a-z0-9-]+[.])?amazonaws[.]com/", url)) {
  url <- sub("^https://s3[.]([a-z0-9-]+[.])?amazonaws[.]com/", "s3://", url)
}
```

**S3 virtual-hosted HTTPS → s3://**

```r
# https://bucket.s3.amazonaws.com/path → s3://bucket/path
# https://bucket.s3.region.amazonaws.com/path → s3://bucket/path
m <- regmatches(url, regexec("^https://([^.]+)\\.s3\\.([a-z0-9-]+\\.)?amazonaws\\.com/(.*)$", url))[[1]]
if (length(m) == 4) {
  url <- paste0("s3://", m[2], "/", m[4])
}
```

**Known custom endpoints — leave as HTTPS.** CEPH, MinIO, Wasabi
URLs can't be translated to `s3://` due to Arrow's bucket name
restrictions. Keep as plain HTTP.

### What it does NOT do

- **Authentication.** `zaro_uri()` only normalises the address. Auth
  (`anonymous = TRUE`, credentials) is still the user's responsibility
  or handled by `zaro()`'s known-public-bucket logic.

- **Path resolution.** It doesn't resolve relative paths, follow
  redirects, or probe the store. It's a string transformation.

- **Catalogue lookup.** It strips catalogue parameters but doesn't
  query catalogues for store URLs. That's intake/STAC territory.

- **Variable extraction.** `::varname=tos` is stripped, not used.
  The user navigates the store with `zaro_meta()`.

## Worked examples

### Pangeo catalogue link → working store

```r
raw <- "https://storage.googleapis.com/cmip6/CMIP6/CMIP/NOAA-GFDL/GFDL-ESM4/historical/r2i1p1f1/Omon/tos/gn/v20180701/::catalog=static/catalog.json::projectionCenterLat=0::projectionCenterLon=0::varname=tos"

zaro_uri(raw)
#> "gs://cmip6/CMIP6/CMIP/NOAA-GFDL/GFDL-ESM4/historical/r2i1p1f1/Omon/tos/gn/v20180701"

store <- zaro(zaro_uri(raw))
#> [zaro] known public bucket 'cmip6', using anonymous access
#> [zaro] opening GCS store via Arrow: gs://cmip6/...
```

### S3 virtual-hosted URL → native S3

```r
raw <- "https://aodn-cloud-optimised.s3.ap-southeast-2.amazonaws.com/model_sea_level_anomaly_gridded_realtime.zarr"

zaro_uri(raw)
#> "s3://aodn-cloud-optimised/model_sea_level_anomaly_gridded_realtime.zarr"

store <- zaro(zaro_uri(raw), anonymous = TRUE, region = "ap-southeast-2")
```

### CEPH URL — no translation

```r
raw <- "https://objects.eodc.eu/e05ab01a9d56408d82ac32d69a5aae2a:sample-data/tutorial_data/cpm_v253/store.zarr"

zaro_uri(raw)
#> "https://objects.eodc.eu/e05ab01a9d56408d82ac32d69a5aae2a:sample-data/tutorial_data/cpm_v253/store.zarr"
# Unchanged — colon in path means it's not standard S3
```

### Plain HTTP — no translation

```r
raw <- "https://ncsa.osn.xsede.org/Pangeo/pangeo-forge/gpcp-feedstock/gpcp.zarr"

zaro_uri(raw)
#> "https://ncsa.osn.xsede.org/Pangeo/pangeo-forge/gpcp-feedstock/gpcp.zarr"
```

## Integration with `zaro()`

Two approaches:

**Option A: Explicit.** User calls `zaro_uri()` when needed.
`zaro()` stays focused on dispatch.

```r
store <- zaro(zaro_uri(messy_url))
```

**Option B: Built-in.** `zaro()` calls the normalisation internally
on every input. Transparent, but harder to debug when the translation
does the wrong thing.

Recommendation: **Option A** initially, **Option B** later once the
rules are well-tested. The normalisation is lossy (stripping query
params) and should be visible to the user until we're confident it's
safe.

## Priority

Medium. The implementation is small (~50 lines of regex). The value
is high for users coming from Python tutorials or catalogue links.
Low risk — it's a pure string transformation with no side effects.
Can ship as a standalone exported function without touching any
existing code paths.

## Broader context

Every Python package in the Zarr ecosystem has an equivalent of this
function buried inside its I/O stack: fsspec's URL parsing, intake's
catalogue resolution, xarray's `open_dataset` argument normalisation,
earthaccess's authentication-aware URL construction. They all solve
the same problem — the gap between "URL someone gave me" and "address
my I/O library needs." In R, making this an explicit, documented,
composable function is more honest than hiding it inside a magic
opener.
