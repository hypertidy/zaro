# zaro: Design Origin Document

_February 2026_

This document records the rationale, questions, decisions, and design choices
that led to the initial zaro package skeleton. It is intended as a persistent
touchstone — the README and user-facing documentation will evolve, but this
record of the founding logic should remain useful for understanding why things
are the way they are.

## Context and motivation

zaro emerged from ongoing work on the ndr package (xarray-like functionality
for R using S7 classes) and its gdalraster Multidim backend. The ndr backend
roadmap already had a working lazy-load-with-`collect()` idiom via GDAL's
Multidimensional API. The question was: what would a *separate*, lower-level
Zarr interface look like if built on Arrow rather than GDAL for the array
semantics?

The impetus was not to replace the GDAL backend for ndr but to explore what
generic Zarr support via Arrow might look like as a composable building block.
The key observations driving this:

1. **Zarr metadata is just text and JSON** — no binary header parsing, no
   format-specific library needed for metadata discovery.

2. **Arrow already has the plumbing** — filesystem abstraction over S3/GCS/local,
   compression codecs (zstd, gzip, lz4, brotli, snappy, bz2), efficient memory
   buffers, and remote data streaming.

3. **Array semantics in memory are simple** — flat vectors with shape and
   orientation metadata suffice much of the time. We don't need Arrow's columnar
   array model for N-dimensional array data; R's native arrays with `dim<-()`
   are adequate.

4. **This is an illustration of what's possible** — part of a grander future
   community consolidation across R's array data ecosystem, not a standalone
   product in isolation.

## What Zarr actually requires

A Zarr store (V2 or V3) reduces to five operations:

- **Metadata resolution** — read `.zarray`/`.zattrs`/`.zgroup` (V2) or
  `zarr.json` (V3) from known key paths, parse JSON.

- **Chunk addressing** — compute the store key for a chunk given its grid
  index. Deterministic string construction from index tuples.

- **Chunk retrieval** — fetch raw bytes for a key from whatever store backend
  is in play (local, S3, HTTP, GCS, or a virtual reference into an archival
  file).

- **Decompression** — run the codec pipeline in reverse. V3 has a formal codec
  chain (array-to-array → array-to-bytes → bytes-to-bytes).

- **Type coercion** — interpret decompressed bytes as a typed array given the
  dtype, endianness, and chunk shape.

There is no query engine, no indexing structure, no relational model. The only
non-trivial logic is chunk-to-slab assembly: determining which chunks intersect
a requested hyperslab, fetching them, decoding, and stitching results into a
single R array. This is pure index arithmetic.

## The R Zarr landscape at time of writing

Three packages matter:

- **zarr** (CRAN, Dec 2025,  R-CF) — Native R implementation,
  Zarr V3 only, extensible store/codec architecture with R6 classes. Local
  filesystem store only. Early stage, modular design. The "proper" community
  effort for V3.

- **pizzarr** (GitHub, keller-mark) — Pure R, Zarr V2 focused, with optional
  blosc via Rarr. HTTP store support. R6-based. More mature but not on CRAN.
  Bioimaging origin.

- **GDAL Zarr driver** (via gdalraster/vapour) — C++ implementation supporting
  V2 and V3, with Kerchunk reference stores, consolidated metadata, all GDAL
  virtual filesystems. This is the ndr Multidim backend and is extremely
  capable but tied to GDAL's abstractions.

- **Copernicus Marine**  another independent engine for CMEMS specifically. 

zaro positions itself as a GDAL-independent layer for array semantics and the
codec pipeline, while being happy to lean on GDAL's filesystem virtualization
as infrastructure (see VSI store discussion below).

## Questions posed and decisions made

### Zarr version scope

**Question:** V2+V3 or V3-only?

**Decision:** V3 only, with a nod to V2 in future. V3 is the current spec, the
zarr CRAN package targets V3, and the V3 metadata model (single `zarr.json`
with explicit codec chains) is cleaner to implement. V2 is where all the
existing data lives, but V2 read support can be added later as a shim without
disrupting the V3-first architecture.

### Initial capability

**Question:** Read-only or read+write from the start?

**Decision:** Read-only first. Writing is a separable concern and the immediate
value is in accessing existing Zarr stores and virtual reference stores. Write
support can follow once the read path is solid.

### API surface

**Question:** Layered S7 classes or flat API?

**Decision:** Keep the API surface flat for now. Four exported functions:
`zaro()`, `zaro_list()`, `zaro_meta()`, `zaro_read()`. Internal S7 classes
provide the structure (stores, metadata) but users interact through the
function-based API. This avoids premature API commitments and keeps the
package approachable.

### Package name

**Decision:** "zaro" — compact, evocative of zarr + arrow without being too
literal. The name `zarrow` was considered but `zaro` is shorter and less likely
to collide.

### Home

**Decision:** Under mdsumner for initial development.

### Reference stores

**Discussion:** Kerchunk JSON and Parquet virtual reference stores were raised
as particularly important. These allow Zarr-style access to existing NetCDF,
HDF5, GRIB, and TIFF archives without reformatting — you get a reference
document that maps Zarr keys to (url, offset, length) tuples into the original
files. This is how VirtualiZarr works and is directly relevant to AAD data
holdings and climate/ocean data workflows.

**Decision:** Reference stores are first-class, not an afterthought. Both
Kerchunk JSON (V0 and V1 with template expansion) and Parquet reference
stores are supported from the initial skeleton.

### HTTP access and gdalraster VSI

**Discussion:** Arrow's C++ library has no HTTP/HTTPS filesystem
(apache/arrow#18980, open since 2021). For HTTP-served Zarr stores (THREDDS,
Copernicus Marine, various data portals), a separate HTTP client would be
needed. The initial analysis proposed httr2/curl as a fallback.

However, gdalraster's VSIFile provides GDAL's entire virtual filesystem layer
— vsicurl with byte-range support, vsis3, vsigs, vsiaz, and more. Rather than
adding httr2 as yet another HTTP client in the stack, gdalraster VSI serves as
a single fallback store backend for anything Arrow doesn't handle natively.

**Decision:** The store layer has two backends: Arrow FileSystem for S3/GCS/local
natively, gdalraster VSI for HTTP and everything else. This reframes the
architecture: zaro is "GDAL-independent for array semantics and the codec
pipeline" but uses GDAL's filesystem virtualization as infrastructure. GDAL's
VSI layer is battle-tested for cloud access patterns in a way that would take
years to replicate.

**Implementation note:** The initial skeleton uses gdalraster's procedural VSI
functions (`vsi_open()`, `vsi_read()`, `vsi_stat()`, `vsi_seek()`,
`vsi_close()`) rather than the `VSIFile` class. This should be refactored to
use gdalraster's `VSIFile` class directly for a cleaner OO interface with
proper `$read()`, `$seek()`, `$tell()` methods.

### Blosc compression

**Discussion:** Blosc is the default compressor for most existing Zarr V2 data
and is common in many V2 stores. Arrow does not support blosc — this is the
biggest codec gap. The `zarr` CRAN package and `pizzarr` both have blosc
support through the `blosc` R package.

**Decision:** Blosc is handled via an optional dependency on the `blosc` R
package with `requireNamespace()` checks. This is not a showstopper — it's a
motivation for deeper compression consolidation in Arrow and in the R
ecosystem more broadly. The V3 spec's move toward zstd as default means the
blosc dependency becomes less critical over time as data providers migrate.

### Dependencies

**Decision:** Minimal required dependencies: `arrow`, `jsonlite`, `S7`. Optional:
`gdalraster` (for HTTP/VSI store access), `blosc` (for blosc-compressed
chunks). This is deliberately lean. The concern is not about dependency count
per se (this is an illustration, not a production hardening exercise) but about
keeping the core tight and the boundaries clear.

## Architecture

Three internal layers, one flat API.

### Layer 1: Store (key-value access)

S7 class hierarchy rooted at `ZaroStore` with three generics: `store_get(key)`
→ raw vector, `store_list(prefix)` → character vector, `store_exists(key)` →
logical.

Three backends:

- `ArrowStore` — wraps `arrow::FileSystem` (LocalFileSystem, S3FileSystem,
  GcsFileSystem). Handles local paths, `s3://`, and `gs://` URIs.

- `VSIStore` — wraps gdalraster VSI functions. Handles `http://`/`https://`
  URIs (via `/vsicurl/`) and anything else GDAL can reach.

- `ReferenceStore` — holds parsed Kerchunk references in memory. Inline
  string data returned directly; remote chunk references dispatched to
  `byte_range_read()` which chains Arrow → gdalraster for the actual I/O.

The `zaro()` constructor dispatches on URI scheme to create the appropriate
store. Special `reference+json://` and `reference+parquet://` schemes trigger
Kerchunk parsing.

### Layer 2: Metadata (JSON → structured representation)

Pure R with jsonlite. Parses `zarr.json` into a `ZaroMeta` S7 class holding
shape, chunks, dtype, codecs, fill_value, dimension_names, attributes. Handles
consolidated metadata by parsing the root document once and attaching the
nested entries.

Key design choice: `jsonlite::fromJSON(simplifyVector = FALSE)` for predictable
handling of heterogeneous codec configurations. This means shape and
chunk_shape come back as lists and need explicit `unlist()` + `as.integer()`,
but avoids surprises with codec configs that have mixed types.

Chunk key computation follows the V3 default encoding:
`c/{idx0}/{idx1}/...` with configurable separator.

### Layer 3: Codec pipeline (decompress + type-cast)

Walks the V3 codec chain in reverse. Each codec in the chain is dispatched
through `decode_one()`:

- **bytes-to-bytes codecs** (compression): Arrow codecs via
  `arrow::Codec$create(type)$Decompress()` for zstd, gzip, lz4, brotli,
  snappy, bz2. Blosc via optional `blosc::blosc_decompress()`.

- **array-to-bytes codecs** ("bytes"/"endian"): identity pass-through at this
  stage; endianness extracted from the codec config and applied at the
  `readBin()` step.

- **array-to-array codecs** ("transpose"): noted but not yet fully implemented;
  would be a `dim<-()` + `aperm()` operation post-read.

Final type coercion uses base R's `readBin()` with dtype-appropriate `what`,
`size`, `signed`, and `endian` arguments. This is direct and avoids an
unnecessary Arrow round-trip for getting data into R vectors.

### Chunk-to-slab assembly

The `zaro_read()` function handles assembling multiple chunks into a single
output array:

1. Compute which chunks intersect the requested hyperslab (`chunk_coverage()`).
2. Allocate output array filled with `fill_value`.
3. Iterate over chunk grid indices (`chunk_index_iter()` via `expand.grid()`).
4. For each chunk: fetch raw bytes, decode through codec pipeline, compute
   overlap region in array coordinates, extract the relevant slice from the
   decoded chunk, insert into the output array.

N-dimensional indexing is handled via `do.call("[", ...)` and
`do.call("[<-", ...)` with dynamically constructed index lists. This works for
arbitrary dimensionality without hard-coding 2D or 3D cases.

Edge chunks (where array shape is not evenly divisible by chunk shape) are
handled by computing the actual chunk extent as
`pmin(chunk_shape, array_shape - chunk_start)` and reshaping the decoded
values accordingly.

R's pass-by-value semantics mean the output array is reassigned on each
`[<-` operation rather than modified in place. For very large reads spanning
many chunks this is suboptimal; a future optimisation could use environments
or external pointers for mutable output buffers.

## Relationship to hypertidy packages

- **tidync** — metadata-first exploration (activate grids, filter dimensions,
  then read). The `hyper_filter()` → `hyper_array()` idiom maps directly to
  "compute chunk indices from dimension filters, fetch and decode chunks,
  assemble array." A zaro store could be another backend for the same UX.

- **ndr** — S7-based xarray-alike. The gdalraster Multidim backend is the
  heavy-lifter. zaro is an alternative backend for GDAL-independent Zarr
  access or direct store-level control (e.g. Kerchunk reference stores,
  custom codec pipelines).

- **vapour** — provides GDAL's virtual filesystem access. For HTTP-served Zarr,
  zaro's VSIStore may lean on the same underlying GDAL infrastructure, but
  through gdalraster rather than vapour.

- **sds** — WMTS catalog system. Zarr stores are increasingly how climate/ocean
  data is served; zaro could simplify the data pipeline for Zarr-native
  sources.

## Known gaps and future work

### VSIFile class refactor

The VSIStore currently uses gdalraster's procedural VSI functions. This should
be refactored to use the `VSIFile` class for a cleaner OO interface.

### Arrow Codec$Decompress() signature

The exact calling convention for `arrow::Codec$Decompress()` needs verification
against the current arrow R package version. The buffer size estimation for
decompression output is currently heuristic (10-20x input size) and may need
adjustment or a more robust approach.

### Sharding (ZEP 2)

Zarr V3 sharding means a single store object contains multiple chunks with an
embedded index. Reading sharded chunks requires parsing the shard index and
doing byte-range reads within a single blob. Arrow's `RandomAccessFile`
supports seek + read, but shard index parsing is not yet implemented.

### Parallel chunk fetching

Arrow's filesystem reads are synchronous from R's perspective. For cloud stores
where latency dominates, parallel chunk fetching via `future`/`mirai` would
significantly improve throughput. The store architecture is compatible with
this — each `store_get()` call is independent.

### V2 read support

Not implemented but the architecture accommodates it. V2 metadata lives in
`.zarray`/`.zattrs`/`.zgroup` rather than `zarr.json`, and chunk keys use
`{i.j}` rather than `c/{i/j}`. The codec model differs (single compressor +
filters vs. V3 codec chain). A `parse_zarray()` function and V2 chunk key
computation would be the main additions.

### Transpose codec

The V3 "transpose" array-to-array codec reorders dimensions before
serialization. On decode, this requires `aperm()` after `readBin()`. The
skeleton notes this but doesn't implement the permutation logic.

### Write support

Not in scope for the initial design. Writing requires the codec chain in
forward order (compress + serialize), chunk key generation, and metadata
serialization. The store interface would need `store_set(key, raw_bytes)`.

## Files in the initial skeleton

```
zaro/
├── DESCRIPTION
├── LICENSE
├── NAMESPACE
├── README.md
├── R/
│   ├── api.R          # flat public API: zaro(), zaro_list(), zaro_meta(), zaro_read()
│   ├── codec.R        # V3 codec pipeline, Arrow decompression, blosc fallback
│   ├── meta.R         # zarr.json parsing, dtype mapping, chunk key computation
│   ├── print.R        # display methods for ZaroMeta and ZaroStore
│   ├── reference.R    # Kerchunk JSON + Parquet reference store parsing
│   ├── store.R        # S7 store classes: ArrowStore, VSIStore, ReferenceStore
│   ├── utils.R        # %||% operator
│   └── zaro-package.R # package-level docs and imports
└── inst/
    └── docs-design/
        └── 001-origin.md  # this document
```

## Principles

These aren't rules, just the positions that shaped the design:

- **Zarr is simple; keep the implementation simple.** The format is a thin
  coordination layer over key-value retrieval, JSON parsing, decompression,
  and buffer reinterpretation. The code should reflect this.

- **Use existing infrastructure.** Arrow for filesystems and codecs, gdalraster
  for VSI, jsonlite for JSON, S7 for classes. Don't reinvent what's already
  battle-tested.

- **Flat vectors with shape metadata suffice.** R's native array model
  (`dim<-()`) is adequate for N-dimensional data. Arrow's columnar model and
  type system are useful for I/O plumbing but not for the in-memory array
  representation.

- **Dependencies are not the concern.** This is an illustration of composability
  across the R ecosystem, not a minimal-dependency exercise. The gaps (blosc,
  HTTP filesystem) are motivations for upstream consolidation, not reasons to
  add more fallbacks.

- **Reference stores are first-class.** Kerchunk JSON and Parquet virtual
  stores are how you bridge the existing archive world (NetCDF, HDF5, GRIB)
  to Zarr-style access patterns. They're not an extension; they're a core
  use case.
