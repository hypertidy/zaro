# zaro (development version)

* Add C style orientation (still upside down for ximage). 

## Core architecture

* Arrow-backed Zarr reader for R. Implements the Zarr specification directly
  using Arrow C++ for filesystem access, compression codecs, and byte buffers.
  No dependency on zarr-python or any Zarr-specific C/Rust library.

* S7 class `ZaroMeta` for parsed array and group metadata, with a uniform
  representation across Zarr V2 and V3.

* Store abstraction layer with `ArrowStore` (local, S3, GCS via Arrow
  `FileSystem`), `VSIStore` (HTTP and fallback S3 via gdalraster),
  `ReferenceStore` (Kerchunk JSON references), and `VirtualiZarrStore`
  (Parquet manifest reference stores). Designed for future backends
  (Icechunk) without changing the read pipeline.

* Codec pipeline normalised across V2 and V3. V2's single compressor plus
  filters list is mapped to V3-style codec chains so the decode path is
  uniform.

## Zarr V2 support

* Parse `.zarray`, `.zgroup`, and `.zattrs` metadata.

* Consolidated metadata via `.zmetadata` — a single HTTP request retrieves
  metadata for all arrays in a V2 store.

* Numpy dtype string parsing (`<f4`, `>i2`, `|u1`, etc.) with endianness
  detection.

* `_ARRAY_DIMENSIONS` convention for dimension names (xarray compatibility).

* `dimension_separator` handling (dot-separated and slash-separated chunk
  keys).

* `fill_value` coercion from JSON strings (`"NaN"`, `"Infinity"`) to R
  numeric values.

* `sanitize_json()` handles Python-flavoured JSON with bare `NaN`,
  `Infinity`, and `-Infinity` values that are invalid in strict JSON.

## Zarr V3 support

* Parse `zarr.json` metadata including codecs, dimension names, fill values,
  and chunk grid configuration.

* Consolidated V3 metadata (single `zarr.json` with embedded node metadata).

## VirtualiZarr Parquet reference stores

* New `VirtualiZarrStore` backend for directory-layout Parquet reference
  stores as produced by VirtualiZarr. These provide Zarr-style access to
  existing archival files (NetCDF, HDF5, GRIB) without reformatting.

* Metadata layer (`.zmetadata`) fetched via curl — GDAL-free.

* Chunk manifests (`refs.N.parq` shards) loaded via `arrow::read_parquet()`
  with lazy per-variable loading and environment-based caching.

* C-order linearization maps chunk grid indices to manifest row positions
  for stores without explicit chunk key columns.

* Inline data support via the `raw` column in manifests (used by coordinate
  variables whose data is stored directly in the Parquet rather than as
  byte-range references).

* Byte-range reads into source files via `byte_range_read()` supporting
  Arrow S3/GCS, gdalraster VSI, and curl for HTTP — the full chain is
  GDAL-free when using curl.

* URI scheme: `virtualizarr://https://example.com/dataset.parq` or
  `virtualizarr:///local/path/dataset.parq`.

* Tested against BRAN2023 ocean temperature (5479 time steps, 51 depth
  levels, 1500x3600 spatial grid, 104 manifest shards, NCI THREDDS
  byte-range serving).

## Cloud access

* S3 stores via Arrow `S3FileSystem` with automatic fallback to gdalraster
  `/vsis3/` on 301 region redirects.

* Anonymous access propagation (`anonymous = TRUE` maps to
  `AWS_NO_SIGN_REQUEST` for both Arrow and GDAL backends).

* GCS stores via Arrow `GcsFileSystem`.

* HTTP/HTTPS stores via gdalraster `/vsicurl/`.

## Reading

* Hyperslab reads with `start`/`count` indexing (0-based, following Zarr
  convention).

* Chunk coverage computation and assembly across chunk boundaries.

* Blosc (lz4, zstd, zlib, snappy), gzip, and zstd decompression via Arrow
  codecs.

* Safety coercion of output array type when fill value would otherwise
  produce a character array.

## Diagnostics

* Verbose messaging throughout (`[zaro] ...`), on by default for
  discoverability. Silence with `verbose = FALSE` or `suppressMessages()`.

* Reports store backend, metadata format detected, dtype/shape/chunks/codecs,
  dimension names, chunk counts during reads.

## Tested against

* AODN cloud-optimised Zarr stores (S3, ap-southeast-2, Zarr V2,
  blosc/lz4 compression, consolidated `.zmetadata`).

* BRAN2023 ocean temperature via VirtualiZarr Parquet references
  (NCI THREDDS byte-range reads, shuffle+zlib compression, int16 with
  scale/offset packing).
