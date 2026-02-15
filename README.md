# zaro

Zarr via Arrow. A low-level Zarr V3 interface for R built on Arrow's filesystem
and codec infrastructure.

## Status

Experimental. This is a design sketch exploring what a lean, composable Zarr
reader looks like when Arrow provides the plumbing.

## Design

Three internal layers, one flat API.

**Store** — key-value access to Zarr stores. Arrow `FileSystem` handles local,
S3, and GCS. gdalraster `VSIFile` handles HTTP and anything else GDAL can
reach. Kerchunk JSON and Parquet reference stores provide virtual Zarr access
to existing archival files.
 
**Metadata** — pure R/jsonlite parsing of `zarr.json` documents (V3). Handles
consolidated metadata, codec chain specifications, chunk key encoding, and
dtype mapping.

**Codec** — decompression pipeline using Arrow codecs (zstd, gzip, lz4,
brotli, snappy, bz2) with optional blosc fallback. Type coercion from raw
bytes to R vectors via `readBin()`.

## API

```r
# open a store
store <- zaro("s3://bucket/dataset.zarr", anonymous = TRUE)
store <- zaro("/local/path/to/store.zarr")
store <- zaro("https://example.com/data.zarr")
store <- zaro("reference+json:///path/to/refs.json")
store <- zaro("reference+parquet:///path/to/refs.parquet")

# explore
zaro_list(store)
zaro_meta(store, "temperature")

# read data (0-based start, count)
data <- zaro_read(store, "temperature",
                  start = c(0, 100, 200),
                  count = c(1, 50, 50))
```

## Dependencies

Required: `arrow`, `jsonlite`, `S7`

Optional: `gdalraster` (HTTP/VSI store access), `blosc` (blosc-compressed
chunks)

## Scope

Zarr V3 only. V2 read support is a future consideration. Read-only for now.

Reference stores (Kerchunk JSON and Parquet) are first-class — these are how
you get Zarr-like access to NetCDF/HDF5/GRIB archives without reformatting.

## Relationship to other packages

- **zarr** (CRAN) — native R Zarr V3 implementation. zaro could contribute an
  Arrow-backed store class to this package.
- **pizzarr** — pure R Zarr V2 implementation.
- **gdalraster** — GDAL Multidim API for Zarr (and NetCDF, HDF5). This is the
  backend for ndr. zaro is GDAL-independent for array semantics but uses GDAL
  VSI for filesystem access.
- **tidync** / **ndr** — higher-level array data interfaces. zaro sits below
  these as potential backend infrastructure.
