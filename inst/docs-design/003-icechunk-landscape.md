# zaro: Icechunk and the Transactional Store Landscape

_February 2026_

This document analyses Icechunk, the transactional storage engine for Zarr
from Earthmover, and its implications for zaro's design and roadmap. It
captures the architectural tension between Zarr's flat-store philosophy and
Icechunk's indirection layer, surveys the current implementation landscape
in Rust and R, and records the design decisions for how zaro should relate
to Icechunk-backed data.

## What Icechunk is

Icechunk is an open-source (Apache 2.0) storage engine built by Earthmover
that sits between Zarr libraries and
object storage. It uses the Zarr data model — arrays, groups, chunks, codecs
— but replaces Zarr's transparent key-to-path mapping with an opaque
indirection layer that enables transactional semantics.

The core capability: serializable isolation between concurrent readers and
writers. Multiple uncoordinated processes can safely read and write the same
data without locks, corruption, or partial visibility. Beyond transactions,
Icechunk provides git-style version control (branches, tags, snapshots with
time travel), efficient virtual dataset support (Kerchunk-style chunk
references stored natively), and chunk sharding.

Icechunk 1.0 was released in July 2025 as a stable spec with backward
compatibility guarantees. It is implemented in Rust (`icechunk` crate) with
Python bindings. The commercial Arraylake platform layers RBAC, garbage
collection, and a catalog UI on top.

## The architectural tension

Zarr's founding principle is that the store is a flat key-value map. The keys
are human-readable paths (`myarray/zarr.json`, `myarray/c/0/0`), and they map
directly to filesystem paths or object keys. Any tool that understands the key
scheme can read the data. No library required — just string construction and
byte fetching.

This is the elegance that makes Zarr fundamentally different from HDF5. It's
why you can write a Zarr V3 reader in an afternoon with jsonlite and readBin.
It's why zaro exists as a viable package at all.

Icechunk replaces this transparent contract with a layer of indirection. The
Icechunk library translates Zarr key requests into actual storage locations
given the user's transactional context — which branch, which snapshot, which
commit. The on-disk layout is not a Zarr store. It consists of:

- **Reference files** — JSON pointers to snapshot IDs, stored per-branch and
  per-tag at known paths (`refs/branch.main/ref.json`, etc.).

- **Snapshot files** — FlatBuffers-encoded trees of node metadata (array
  shapes, chunk shapes, Zarr metadata) plus pointers to manifest files.

- **Manifest files** — FlatBuffers-encoded chunk references. Each reference
  can be native (pointing to a chunk file within the repo), inline (small
  chunks embedded directly), or virtual (pointing to byte ranges in external
  files like NetCDF/HDF5).

- **Chunk files** — the actual compressed binary chunks. These can be one
  chunk per file (standard Zarr layout), multiple chunks packed into a single
  file (sharding), or chunks from multiple arrays sharing a file.

- **Transaction logs** — FlatBuffers-encoded records of per-commit
  modifications, used for conflict resolution and diff.

Object IDs are content-addressed in Crockford base32. You cannot point a
naive Zarr reader at an Icechunk repo and get data — you need something that
can resolve the branch → snapshot → manifest → chunk location chain.

## The HDF5 parallel

This is worth stating plainly: Icechunk's architecture recapitulates the
move HDF5 made decades ago. Rich data model, opaque container format,
mandatory library for access. The consequences of that design are well known:
incredible capability, poor interoperability, libhdf5 as a permanent and
sometimes painful dependency, and — eventually — Zarr was created
specifically to escape that coupling.

Icechunk is more open than HDF5 ever was (published spec, Apache 2.0 Rust
library, FlatBuffers IDL available for inspection). And the problems it
solves — concurrent write safety, transactional rollback, efficient
versioning — are real engineering gaps in plain Zarr stores on S3. But the
structural parallel is there: access requires a specific library, the
on-disk format is not self-describing to a generic reader, and the spec is
effectively defined by a single implementation.

Earthmover's FAQ explicitly addresses this: they recommend binding to the
Rust library rather than reimplementing from scratch, and acknowledge that
only one independent read-only implementation exists (Google's Neuroglancer).
This is honest, but it confirms the practical situation: Icechunk access
means the Rust library, one way or another.

## The zarrs ecosystem and Icechunk

The zarrs Rust crate (Lachlan Deakin, ANU Canberra) is the most complete
and performant Zarr V3 implementation in Rust. It has a modular architecture
with stores as separate crates:

- **zarrs** — core library, supports filesystem, in-memory, HTTP, and a huge
  range of object storage backends via opendal and object_store crates.

- **zarrs_icechunk** — separate crate that wraps an Icechunk session as an
  `AsyncIcechunkStore`. This is *not* a hard dependency of zarrs. You pull
  it in when you want it; otherwise zarrs works with plain stores
  independently.

- **zarrs_ffi** — C/C++ bindings exposing a subset of the zarrs API as a
  single-header library (`zarrs.h`). Currently covers array retrieval (get
  chunk, get subset, get shape, metadata) but only a small subset of the
  full API.

- **zarrs-python** — a drop-in high-performance codec pipeline for
  zarr-python, backed by zarrs.

This modularity matters. The supplementary crates are separated from zarrs
specifically to stay up-to-date with unstable public dependencies (opendal,
object_store, icechunk) without impacting the zarrs release cycle. Icechunk
is treated as an optional, potentially volatile dependency — exactly the
right posture.

zarrs is developed at ANU — an Australian institution with geographic and
institutional proximity to AAD. The maintainer would be a natural
collaborator on R bindings.

## Paths to Icechunk from R

There are three realistic paths for accessing Icechunk-backed data from R:

### Path 1: extendr wrapping zarrs + zarrs_icechunk

Wrap the full zarrs Rust API via extendr, including the Icechunk store
backend. This gives access to all codecs, all store backends, async I/O,
and Icechunk's transactional features (branches, snapshots, time travel).

**Advantages:** Full capability. Single dependency chain. zarrs is the
fastest Zarr implementation available and would bring its performance to R.

**Disadvantages:** Heavy compile-time dependency. zarrs + tokio + icechunk +
codec dependencies is a substantial Cargo tree to build from source during
R package installation. extendr's async story is still maturing.

### Path 2: zarrs_ffi (C API) → R .Call()

Link against the zarrs_ffi shared library and call through R's C interface.
No extendr needed — just a compiled .so/.dll and .Call() wrappers.

**Advantages:** Thinnest FFI boundary. No Rust compiler needed at R install
time if pre-built binaries are distributed (as polars does).

**Disadvantages:** zarrs_ffi currently exposes only a small subset of the
zarrs API. It does not yet expose Icechunk store creation. The C API surface
would need to grow substantially to be useful for zaro's needs.

### Path 3: extendr wrapping icechunk-rs alone

If zaro only needs Icechunk for store-level access (resolve keys to bytes),
wrap just the icechunk crate. Use it for snapshot resolution, manifest
traversal, and chunk byte fetching, then feed raw bytes into zaro's existing
R-native codec pipeline.

**Advantages:** Narrower dependency than full zarrs. zaro retains its own
codec pipeline and array semantics — only the store layer delegates to Rust.

**Disadvantages:** Still brings in icechunk's full dependency tree (object_store,
FlatBuffers, tokio). Duplicates codec work that zarrs already does well.

### Assessment

Path 1 is the most capable but makes zaro essentially a thin R wrapper
around zarrs, which changes the package's character. Path 3 is the most
natural fit for zaro's architecture — it keeps the store layer as the
integration point and preserves zaro's R-native codec pipeline and array
assembly. Path 2 is attractive for distribution but premature given
zarrs_ffi's current API surface.

The recommended path is **Path 3 initially, Path 1 as the ecosystem
matures.** Concretely: an `IcechunkStore` backend in zaro that wraps
icechunk-rs (via extendr or a dedicated icechunkr package) for store-level
access only. If/when zarrs_ffi grows to expose Icechunk stores and richer
array operations, or a mature R-zarrs binding emerges, zaro can delegate
more functionality downstream.

## How this fits zaro's architecture

zaro's store abstraction was designed for exactly this extension. The three
current backends — ArrowStore, VSIStore, ReferenceStore — each implement
`store_get(key)`, `store_list(prefix)`, and `store_exists(key)`. An
IcechunkStore backend would:

1. **Open a repository** — take a path/URI plus branch/tag/snapshot
   specification, initialize an icechunk Session via the Rust library.

2. **Resolve Zarr keys** — translate `store_get("myarray/zarr.json")` or
   `store_get("myarray/c/0/0")` through the Icechunk indirection layer
   (session → snapshot → manifest → chunk location).

3. **Return raw bytes** — hand back the chunk bytes to zaro's existing
   codec pipeline, which handles decompression and type coercion identically
   to any other store backend.

The codec pipeline, chunk-to-slab assembly, metadata parsing, and
parallelization strategy all remain unchanged. The IcechunkStore is just
another implementation of the store interface.

### Store serialization for parallel workers

The parallelization design (002-parallelization.md) already identified the
store serialization challenge: ArrowStore wraps R6/C++ objects that don't
serialize across process boundaries. The solution — a `store_spec` pattern
where stores describe themselves as serializable specifications — applies
equally to IcechunkStore. Workers would reconstruct fresh Icechunk sessions
from the repository URI, branch, and snapshot ID.

For Icechunk specifically, there's a subtlety: Icechunk sessions are
stateful (they track which snapshot they're reading from). Read-only
sessions are safe to reconstruct on workers as long as the snapshot ID is
pinned. The store_spec for an IcechunkStore would include the resolved
snapshot ID, not just the branch name, to ensure workers read from a
consistent view.

### URI scheme

Following the existing `reference+json://` and `reference+parquet://` scheme
pattern, Icechunk repos could use:

```r
store <- zaro("icechunk+s3://bucket/repo.zarr", branch = "main")
store <- zaro("icechunk+file:///path/to/repo.zarr", tag = "v1.0")
store <- zaro("icechunk+s3://bucket/repo.zarr", snapshot = "8GNCC3M3...")
```

The `icechunk+` prefix signals that the Icechunk library is needed for
store resolution. The underlying storage URI (s3://, file://, gs://) tells
Icechunk which object store backend to use.

## GDAL integration prospects

A GDAL Icechunk driver would provide another path: access Icechunk repos
via gdalraster, leveraging GDAL's existing Zarr driver infrastructure. This
would make Icechunk data accessible through gdalraster's VSI/Multidim API
without any zaro-specific code.

However, this faces significant hurdles:

- **Rust dependency.** GDAL currently has no Rust dependencies. Adding
  icechunk-rs (via C FFI) would be a first and would require PSC scrutiny
  under RFC 85's policy on substantial code additions and binary SDK
  dependencies.

- **Maintenance commitment.** RFC 85 requires that contributors of
  significant code additions participate in day-to-day GDAL project
  maintenance, monitor issue trackers and mailing lists, and support their
  contributions long-term. Earthmover would need to commit to this.

- **Community dynamics.** GDAL's existing Zarr V2/V3 driver is comprehensive and battle-tested. Adding an Icechunk
  driver would need to complement rather than compete with the existing
  Zarr infrastructure.

Conversations between Earthmover and the GDAL PSC have reportedly occurred
but no concrete RFC or pull request has emerged. This path is plausible but
not imminent. For zaro's planning, it should be treated as a future
possibility rather than a dependency.

## The ideal outcome

The healthiest outcome for the ecosystem would be standardization of
transactional semantics at the Zarr spec level (via a ZEP) rather than
requiring a specific storage engine. This would allow any Zarr
implementation — including zaro — to support transactional features through
the standard key-value interface, with Icechunk as one implementation among
potentially several.

Earthmover's FAQ notes that they may propose Icechunk as a Zarr extension
in the future, but acknowledge that because it sits below Zarr in the stack,
the integration path is unclear. The governance dynamics between the Zarr
spec community and Earthmover's commercial interests (Arraylake) are worth
watching.

## Summary: zaro's Icechunk roadmap

**Now:** zaro reads plain Zarr V3 stores and Kerchunk reference stores.
These are the flat, transparent, specification-driven formats. No Icechunk
dependency.

**Next:** When Icechunk-backed data holdings become important to access from
R (increasingly likely as Earthmover's adoption grows in the climate/ocean
community), add an IcechunkStore backend. This would be gated behind an
optional dependency on a hypothetical `icechunkr` package (wrapping
icechunk-rs via extendr). The store interface in zaro is already designed
for this — only `store_get()`, `store_list()`, and `store_exists()` need
implementing.

**Later:** If zarrs_ffi matures to expose Icechunk stores and richer array
operations, or if a GDAL Icechunk driver emerges, zaro can delegate to
those paths instead. The store abstraction makes backend swaps transparent
to downstream code.

**Always:** Everything zaro does without Icechunk remains GDAL-independent,
Arrow-native, and specification-driven. Plain Zarr V3 and Kerchunk
reference stores are the common denominator. Icechunk is an optional
extension, not a prerequisite.
