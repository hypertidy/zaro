# zaro 0.0.5

## Core architecture

* Arrow-backed Zarr reader for R. Implements the Zarr specification directly
  using Arrow C++ for filesystem access, compression codecs, and byte buffers.
  No dependency on zarr-python or any Zarr-specific C/Rust library.

* S7 class `ZaroMeta` for parsed array and group metadata, with a uniform
  representation across Zarr V2 and V3.

* Store abstraction layer with `ArrowStore` (local, S3, GCS via Arrow
 `FileSystem`) and `VSIStore` (HTTP and fallback S3 via gdalraster). Designed
  for future backends (Icechunk, Kerchunk references) without changing the
  read pipeline.

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

## Zarr V3 support

* Parse `zarr.json` metadata including codecs, dimension names, fill values,
  and chunk grid configuration.

* Consolidated V3 metadata (single `zarr.json` with embedded node metadata).

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

## Diagnostics

* Verbose messaging throughout (`[zaro] ...`), on by default for
  discoverability. Silence with `verbose = FALSE` or `suppressMessages()`.

* Reports store backend, metadata format detected, dtype/shape/chunks/codecs,
  dimension names, chunk counts during reads.

## Tested against

* AODN cloud-optimised Zarr stores (S3, ap-southeast-2, Zarr V2,
  blosc/lz4 compression, consolidated `.zmetadata`).
