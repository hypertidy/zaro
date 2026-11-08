# zaro (development version)

* `mdimrefs://` accepts stores stamped `byterefs/0.1`, the identifier
  raad.virtualize writes since the format was renamed (byterefs/spec
  decision 0003), as well as the legacy `mdim-refs/0.2`. Such stores were
  refused with "not an mdim-refs store".

* Sharded arrays with codecs after `sharding_indexed` (applied to the whole
  shard object) now read. A trailing `crc32c` is skipped and the shard is
  still read by byte ranges; a compressor means whole shards are fetched and
  decoded at plan time, and their inner chunks become inline plan rows.
  These codecs were previously dropped, so such stores failed with
  length errors. `transpose` before sharding and nested sharding remain
  unsupported, with clear errors.

* A V3 `data_type` given as an object (`{"name": ...}`) keeps its name, so
  unsupported types fail with "unsupported dtype" rather than a class
  validation error.

* Acceptance: the EOPF Sentinel-2 L2A sharded GeoZarr on
  s3.explorer.eopf.copernicus.eu (blosc/zstd inner chunks, index at the end
  with crc32c) reads identically to zarr-python 3.2.1 and GDAL 3.14; the
  seven readable sharded fixtures in GDAL's autotest read as GDAL reads
  them (tests run when `ZARO_GDAL_AUTOTEST` or `~/gdal/autotest` is found).

* Read sharded Zarr V3 arrays (`sharding_indexed`). A shard is planned as
  one more level of byte references: `zaro_plan()` reads the index of each
  shard a selection touches (one small ranged read per shard, fetched
  together; a suffix range when the index is at the end) and turns every
  inner chunk into a byte range of its shard object. Empty inner chunks and
  missing shards read as `fill_value`. The index's `crc32c` is stripped, not
  verified. Sharded arrays present their inner chunk shape as the array's
  chunks, so `zaro_read()`, `zaro_array()` and `zaro_chunk()` work
  unchanged; `print()` of the metadata shows the shard shape. Checked
  against arrays written by zarr-python 3 (zstd and gzip inner codecs,
  index at end and start, partially written arrays).

* A `crc32c` codec on ordinary chunks is stripped (not verified); it was
  passed through as an unknown codec.

* Fix V3 chunk keys: the `"v2"` chunk key encoding (no `c` prefix, default
  separator `"."`) and the `"."` separator of the default encoding
  (`c.0.1`) were written as `c/...`.

* New `zaro_fetch(store, plan)` executes a plan: HTTP references go through
  one curl connection pool (`options(zaro.max_con)`, default 8, per host
  and in total), byte ranges that touch on the same URL are merged into one
  request (`options(zaro.merge_gap)` widens that), and failed or short
  responses are retried with backoff. `zaro_read()` and `zaro_array()` read
  through plan + fetch, so every altarr batch is one pooled fetch. The
  BRAN2023 30-point track reads in 0.9 s (4.8 s one connection at a time);
  a 60-chunk BRAN2023 surface slice is 2 requests.

* `http://` and `https://` stores are read with curl (`HttpStore`), not
  gdalraster: GDAL-free, metadata cached, chunks fetched through the pool.
  An HTTP store can't be listed, so it needs consolidated metadata (or
  `zaro_dataset(arrays = )`). HTTP errors other than not-found are errors,
  not silently absent chunks. Any `/vsi...` path opens a gdalraster
  `VSIStore`, e.g. `zaro("/vsicurl/https://...")` to keep GDAL's HTTP
  handling.

* New `zaro_plan()`: the byte references a selection needs, before any
  read. One row per chunk with `url`, `offset`, `length` and `transport`
  (whole object, byte range in a NetCDF/HDF5 file, inline, or absent), and a
  printed cost summary (requests, bytes, hosts). Native stores plan with no
  request; virtual stores read only the manifest they need.

* Every store now answers `store_refs()`, and the virtual stores
  (kerchunk, VirtualiZarr, mdim-refs) fetch through it, so native and
  virtual reads share one path ("everything is a refs table").

* Kerchunk inline chunks prefixed `base64:` are decoded; references with
  only a URL read the whole object.

* `byte_range_read()` reads local files with base R (no GDAL), and its
  `s3://`/`gs://` branches return raw vectors (they returned Arrow Buffers).

* Fix decoding of `int64`, `uint64`, `uint32` and `float16` arrays, which
  had never been read correctly: the 8- or 4-byte integers were
  reinterpreted as floating point (e.g. CMIP6 `time`, stored as int64 hours,
  read as values like 3.5e-321) and float16 failed. 64-bit integers and
  uint32 now read as exact doubles (up to 2^53), float16 is decoded from its
  bit fields, in either byte order.

* `verbose` defaults to `getOption("zaro.verbose", TRUE)` in every
  function, so `options(zaro.verbose = FALSE)` silences zaro everywhere;
  an explicit `verbose =` still wins.

* Package source, docs and NEWS are ASCII only.

* `R CMD check` is clean: curl, methods and utils are declared in Imports
  (curl will be the batched fetcher), `.github` and other dev files are
  build-ignored, and the track-extract demo moved from `inst/demo` (a
  directory R reserves) to `inst/examples`.

* Fix remote kerchunk JSON never reading through gdalraster: the VSI file
  was opened without its path, so the read always failed over to `url()`.

* `parse_zarray()` passed an undefined `data_type` when coercing the fill
  value; it now passes the parsed dtype.

* New `zaro_dataset()`: every array at the root of a store, with dimension
  names and sizes in R order and the root attributes, read from consolidated
  metadata in one request (or by listing and probing the store). No data is
  read. This is the hand-off to ndr's `open_dataset(engine = "zaro")`.

* `zaro_array()` gains `cf = TRUE`: fill and missing values read as `NA`,
  then `scale_factor`/`add_offset` are applied per chunk. It also accepts a
  `zaro_dataset` in place of a store.

* Fix consolidated metadata losing numeric precision. Entries were
  re-serialized with jsonlite's default of 4 significant digits, so e.g.
  BRAN2023's `scale_factor` of 0.007782219909131527 became 0.0078 and
  unpacked values were off by up to 0.53 degC. Numbers now round-trip
  exactly (V2 `.zmetadata`, V3 consolidated `zarr.json`, mdim-refs).

* Fix print methods not being used outside the package: the S7 methods are
  now registered in `.onLoad()`, and a stray namespace copy of `print` no
  longer captures the S3 registration. Printing an array of a
  variable-length type (`string`) no longer errors.

* New `mdimrefs://` source: `zaro()` reads an mdim-refs SQLite byte-reference
  table directly, serving Zarr V2 keys from its `arrays` table and
  `refs_<array>` views (`target` selects a path remap).

* Fix `zaro_read()` returning a list-array when `fill_value` is null. A null
  fill value (common for coordinate arrays) is now coerced to `NA`, so reads
  return a plain numeric array. This also covers the case where a V2
  consolidated-metadata round-trip degrades `null` to an empty object `{}`,
  and the V3 path, which previously did not coerce `fill_value` at all.

* Add chunk facilites `zaro_chunk()`, `zaro_chunks()`, `zaro_chunk_apply()`, `zarr_chunk_info()`. 

* Fix problem with Arrow decompression (logic was incorrect and hidden in a try/catch). 

* Checks now in place for source strings. 

* Optional validation by reading metadata on connect with zaro() (off by default). 

* Use parallel read with mirai settings for zaro_read.

* Add helpers for common cases. 

* Improved print for meta group. 

* Fixed *egregious* error on how to use VSIFile hidden within tryCatch(). 

* Add C style orientation (still upside down for ximage). 

# zaro 0.0.1

Initial version.

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

* Consolidated metadata via `.zmetadata` - a single HTTP request retrieves
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

* Metadata layer (`.zmetadata`) fetched via curl - GDAL-free.

* Chunk manifests (`refs.N.parq` shards) loaded via `arrow::read_parquet()`
  with lazy per-variable loading and environment-based caching.

* C-order linearization maps chunk grid indices to manifest row positions
  for stores without explicit chunk key columns.

* Inline data support via the `raw` column in manifests (used by coordinate
  variables whose data is stored directly in the Parquet rather than as
  byte-range references).

* Byte-range reads into source files via `byte_range_read()` supporting
  Arrow S3/GCS, gdalraster VSI, and curl for HTTP - the full chain is
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
