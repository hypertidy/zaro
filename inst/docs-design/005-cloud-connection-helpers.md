# Cloud Store Connection Helpers

_March 2026_

## The problem

Scientific Zarr data increasingly lives on cloud object storage. Python
tutorials show how to access it using fsspec, s3fs, and boto3 — but the
configuration surface in Python is large and fragmented. R users
encountering these examples face an undocumented translation problem.

The underlying reality is simpler than Python makes it look. Nearly all
scientific data falls into three cases, and zaro already handles two of
them. This document records a worked example of the hard case, and
proposes lightweight helpers for future work.

## The three cases

**Case 1: Standard S3/GCS, public.** The bucket is on AWS or Google
Cloud and allows anonymous access. This is AODN, CMIP6, Pangeo Forge,
NOAA, NASA, most climate/ocean data.

```r
store <- zaro("s3://aodn-cloud-optimised/dataset.zarr",
              anonymous = TRUE, region = "ap-southeast-2")
store <- zaro("gs://cmip6/CMIP6/.../v20180701")  # auto-anonymous
```

Already works. Known public buckets are auto-detected.

**Case 2: Standard S3/GCS, authenticated.** The bucket requires
credentials. AWS credentials come from environment variables, IAM
roles, or `~/.aws/credentials`. GCS from `GOOGLE_APPLICATION_CREDENTIALS`
or `gcloud auth`.

```r
store <- zaro("s3://private-bucket/dataset.zarr")
```

Already works — Arrow picks up credentials from the environment.

**Case 3: Custom S3-compatible endpoint.** The data is on CEPH, MinIO,
Wasabi, or another S3-compatible service that isn't AWS. This is where
things get interesting.

## Worked example: EODC CEPH storage

### Starting point: Python code

We encounter this in a tutorial:

```python
import s3fs

bucket = "e05ab01a9d56408d82ac32d69a5aae2a:sample-data"
prefix = "tutorial_data/cpm_v253/"
prefix_url = "https://objects.eodc.eu"

fs = s3fs.S3FileSystem(anon=True, client_kwargs={"endpoint_url": prefix_url})

# unregister handler to make boto3 work with CEPH
handlers = fs.s3.meta.events._emitter._handlers
handlers_to_unregister = handlers.prefix_search("before-parameter-build.s3")
handler_to_unregister = handlers_to_unregister[0]
fs.s3.meta.events._emitter.unregister(
    "before-parameter-build.s3", handler_to_unregister
)

s3path = "s3://" + f"{bucket}/{prefix}" + "S2A_MSIL2A_*_*_*_T32UPC_*.zarr"
remote_files = fs.glob(s3path)
```

### What we learn from the Python code

- `anon=True` → anonymous access, no credentials needed
- `client_kwargs={"endpoint_url": ...}` → custom S3 endpoint, not AWS
- The boto3 event handler hack → CEPH-specific workaround, Python-only
- `bucket` contains a colon → CEPH tenant:bucket naming convention
- The glob pattern → need to discover actual store names

### Attempt 1: Direct S3 URI

The natural translation:

```r
store <- zaro("s3://e05ab01a9d56408d82ac32d69a5aae2a:sample-data/tutorial_data/cpm_v253/store.zarr",
              anonymous = TRUE,
              endpoint_override = "https://objects.eodc.eu")
```

Fails: Arrow's S3 client rejects the colon in the bucket name.

```
Error: Invalid: Expected an S3 object path of the form 'bucket/key...',
got a URI: 'e05ab01a9d56408d82ac32d69a5aae2a:sample-data/...'
```

Arrow interprets the colon as a URI scheme separator. This is a known
limitation — CEPH's tenant:bucket naming convention is not standard S3.

### Attempt 2: HTTP fallback

Since it's anonymous anyway, the S3-compatible endpoint serves data
over plain HTTPS. The bucket and key are just URL path components:

```r
store <- zaro("https://objects.eodc.eu/e05ab01a9d56408d82ac32d69a5aae2a:sample-data/tutorial_data/cpm_v253/store.zarr")
```

This constructs a `/vsicurl/` store via gdalraster. But we need a real
store name — `store.zarr` was a placeholder.

### Finding the actual store names

S3-compatible endpoints support the ListObjects XML API over HTTP.
The bucket name (including the colon) is just a URL path:

```r
url <- paste0(
  "https://objects.eodc.eu/",
  "e05ab01a9d56408d82ac32d69a5aae2a:sample-data",
  "?prefix=tutorial_data/cpm_v253/&delimiter=/&max-keys=20"
)
cat(rawToChar(curl::curl_fetch_memory(url)$content))
```

This returns an XML listing with actual `.zarr` directory names.

### Working solution

With a real store name from the listing:

```r
base <- "https://objects.eodc.eu/e05ab01a9d56408d82ac32d69a5aae2a:sample-data"
store <- zaro(paste0(base, "/tutorial_data/cpm_v253/S1A_IW_GRDH_1SDV_20220906T135439_20220906T135504_044885_055C76_B53F.zarr"))
meta <- zaro_meta(store)
# [zaro] found .zmetadata (Zarr V2 consolidated)
# [zaro]   150 arrays: ...
```

It works. The colon is fine in a plain URL — it's only problematic
when Arrow tries to parse it as an S3 bucket name.

### Key insight

The Python code needed 10 lines of configuration including a boto3
internal event handler hack. The R solution is a plain HTTPS URL.
The colon-in-bucket problem and the CEPH compatibility workaround
are both Python/boto3-specific. GDAL's `/vsicurl/` doesn't care — it's
just an HTTP GET.

## Python-to-R translation reference

| Python (fsspec/s3fs/boto3) | zaro equivalent |
|---|---|
| `anon=True` | `anonymous = TRUE` |
| `client_kwargs={"endpoint_url": "..."}` | `endpoint_override = "..."` (or plain HTTP URL) |
| `s3fs.S3FileSystem(...)` | `zaro("s3://...")` |
| `fsspec.filesystem("gcs", token="anon")` | `zaro("gs://...", anonymous = TRUE)` |
| `storage_options={"anon": True}` | `anonymous = TRUE` |
| CEPH `tenant:bucket` | Use HTTP URL directly |
| `s3fs.S3FileSystem.glob(...)` | `store_list()` or S3 ListObjects XML via curl |

## Proposed helper: `zaro_s3()`

For cases where the S3 URI can't be used directly:

```r
zaro_s3 <- function(bucket, key = "", endpoint = NULL,
                    region = NULL, anonymous = TRUE) {
  # CEPH tenant:bucket — fall back to HTTP
  if (grepl(":", bucket)) {
    base <- endpoint %||% "https://s3.amazonaws.com"
    url <- paste0(base, "/", bucket, "/", key)
    return(zaro(url))
  }

  args <- list(source = paste0("s3://", bucket, "/", key),
               anonymous = anonymous)
  if (!is.null(endpoint)) args$endpoint_override <- endpoint
  if (!is.null(region)) args$region <- region
  do.call(zaro, args)
}
```

Usage:

```r
store <- zaro_s3(
  bucket = "e05ab01a9d56408d82ac32d69a5aae2a:sample-data",
  key = "tutorial_data/cpm_v253/S1A_IW_GRDH_1SDV_20220906T135439_20220906T135504_044885_055C76_B53F.zarr",
  endpoint = "https://objects.eodc.eu"
)
```

## Priority

Low. The existing `zaro()` handles cases 1 and 2. Case 3 is rare and
the HTTP URL workaround is simple once you know it. A vignette
documenting the translation patterns is higher value than the helper
function — the hard part is knowing to try HTTP, not typing the URL.

## Broader context

The Python ecosystem makes cloud access feel complicated because it
has four overlapping I/O stacks (fsspec, boto3, s3fs, aiobotocore)
each with their own configuration surface. The underlying protocols
are simple: S3 is HTTP with auth headers, GCS is HTTP with OAuth,
and CEPH/MinIO are HTTP with S3-compatible auth. In R, Arrow's C++
client handles standard S3/GCS, and plain HTTP URLs handle everything
else. Two paths, not four libraries.
