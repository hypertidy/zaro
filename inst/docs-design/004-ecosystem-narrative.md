# zaro: Ecosystem Position and the Interoperability Argument

_February 2026_

This document articulates how zaro fits into the broader R geospatial and
scientific computing landscape, and makes the case for interoperability
through shared specification rather than language-specific bindings. It is
intended as source material for the hypertidy project narrative — explaining
why these packages exist, what they share with the Python ecosystem, and
where the R community has undersold its own capabilities.

## The misconception

A common view in the scientific computing community: Python has xarray, Zarr,
Dask, and a coherent cloud-native data stack. R has some spatial packages
but they're weird and bespoke, can't handle modern formats, and don't work
at scale.

This is wrong, and the existence of zaro — built in an afternoon from
composable R infrastructure — is one illustration of why.

## What R actually has

The following capabilities exist in R today and are backed by the same
underlying C/C++/Rust libraries as their Python equivalents:

**Arrow.** The `arrow` R package is a first-class binding to the same Apache
Arrow C++ library as PyArrow. It provides FileSystem abstraction over S3,
GCS, and local storage; compression codecs (zstd, gzip, lz4, brotli,
snappy, bz2); efficient memory buffers; and Parquet/IPC/CSV I/O. This is
not a thin wrapper — it exposes the full breadth of Arrow's compute and I/O
infrastructure.

**GDAL.** The `gdalraster` package gives R the same GDAL C++ library as
Python's rasterio and osgeo.gdal, with a clean modern interface that maps
closely to GDAL's C API. GDAL's virtual filesystem layer (vsicurl, vsis3,
vsigs, vsiaz), its Multidimensional API for Zarr/NetCDF/HDF5, and its
raster/vector drivers are all accessible. The `vapour` package provides
an alternative lightweight GDAL interface focused on direct access patterns.

**Parallel computing.** The `mirai` package's C-level dispatcher has
per-task overhead in the tens of microseconds — competitive with Python's
concurrent.futures and Dask's task scheduler. mirai is now the recommended
async backend for Shiny, the only async backend for plumber2, the parallel
backend for purrr, and an official alternative communications backend for
R's `parallel` package. The Futureverse (future, futurize, future.apply)
provides backend-independent parallelism that scales from local cores to
Slurm clusters with a one-line change.

**Type system.** S7, R's modern class system (successor to S4 and R5/R6),
provides formal generics, multiple dispatch, property validation, and
clean inheritance. It is arguably more principled than Python's ad-hoc class
hierarchy, which relies on conventions (dunder methods, ABCs) rather than
language-level enforcement.

**Array semantics.** R's native array model — flat vectors with `dim<-()`
and `aperm()` — is simple and adequate for N-dimensional data. It doesn't
have xarray's labeled-dimension ergonomics built in, but that's a thin
layer (the ndr package) over the same fundamental representation.

## Interoperability through specification

The key insight: these capabilities don't interoperate because they share a
language. They interoperate because they share *specifications*.

A climate scientist writes Zarr V3 data from xarray in Python, generates
Kerchunk references with VirtualiZarr, and publishes them to S3. An R user
reads the same data with zaro — parsing the same JSON metadata, applying
the same codec chain, fetching bytes through the same S3 protocol, and
assembling the same array. No format conversion, no language-specific
serialization, no bridge library.

This works because Zarr V3 is a specification, not a library. Arrow's
on-disk formats (Parquet, IPC) are specifications. S3 is a protocol. The
codec algorithms (zstd, gzip, blosc) are specifications with multiple
implementations. JSON is a specification.

zaro demonstrates this concretely: its metadata layer uses jsonlite (R's
JSON parser), its codec layer uses Arrow's C++ codecs (the same ones
PyArrow uses), its store layer uses Arrow's C++ filesystem abstraction
(the same one PyArrow uses) or GDAL's VSI layer (the same one rasterio
uses), and its output is a plain R array. Every component talks to its
Python equivalent through shared specification, not through bindings.

## Where hypertidy packages sit

The hypertidy ecosystem — tidync, vapour, sds, raadtools, ndr, and now
zaro — represents a coherent vision for scientific array data in R. The
packages aren't a monolith; they're composable primitives with different
abstraction levels:

**Data access primitives:**
- `vapour` — direct GDAL access (raster values, vector features, metadata)
- `gdalraster` — comprehensive GDAL interface (VSI, Multidim, warping)
- `zaro` — GDAL-independent Zarr access via Arrow

**Metadata and exploration:**
- `tidync` — metadata-first NetCDF exploration (activate, filter, read)
- `sds` — WMTS/data catalog discovery and tile access

**Higher-level abstractions:**
- `ndr` — xarray-like labeled arrays with S7 classes and lazy evaluation
- `raadtools` — domain-specific readers for Antarctic/ocean research data

**Utilities:**
- `reproj` — lightweight coordinate reprojection
- `controlledburn` — sparse polygon rasterization

The design principle throughout: opinions at the top, primitives at the
bottom. tidync has strong opinions about how you explore NetCDF files
(activate grids, filter dimensions, hyper_array). vapour has almost none —
it gives you raw access and gets out of the way. zaro sits in the
primitives layer: it reads Zarr stores and gives you arrays. What you do
with them is your business.

## The comparison to Python's stack

Python's xarray/Zarr/Dask stack is excellent. It provides a coherent,
well-documented, heavily-used pipeline from cloud storage through lazy
array operations to computed results. The ecosystem has benefited enormously
from institutional investment (Pangeo, Earthmover, 2i2c, CarbonPlan) and
from being the first mover in cloud-native scientific data.

But the stack is also opinionated in ways that aren't always acknowledged:

- xarray's data model assumes CF conventions and labeled dimensions. This
  is great for climate data; it's a mismatch for imagery, point clouds,
  unstructured meshes, and tracking data.

- Dask's task graph scheduler is powerful but adds significant complexity
  to debugging and performance tuning. Many users would be better served by
  simpler parallelism (mirai, future) with explicit control over task
  granularity.

- fsspec, the Python filesystem abstraction, is pure Python and notably
  slower than Arrow's C++ FileSystem or GDAL's VSI for high-throughput
  cloud I/O. Icechunk was partly motivated by the need to replace fsspec's
  performance characteristics.

- The stack's tight integration means that deviating from the expected
  workflow (xarray → Dask → Zarr → S3) can be surprisingly difficult.
  Trying to use Zarr without xarray, or Dask without Zarr, requires more
  expertise than the tutorials suggest.

The hypertidy approach is different: loosely coupled packages that compose
through R's standard interfaces (vectors, arrays, data frames, functions).
You can use tidync without ndr, zaro without tidync, vapour without either.
The cost is less out-of-the-box integration; the benefit is that each piece
is independently useful and independently replaceable.

## The communication problem

The R geospatial community has not told this story well.

The capability exists. gdalraster gives R the same GDAL that rasterio gives
Python. arrow gives R the same Arrow that PyArrow gives Python. mirai gives
R parallel computing competitive with anything in Python's ecosystem. S7
gives R a class system more principled than Python's. zaro reads the same
Zarr V3 stores that zarr-python reads.

But these capabilities are distributed across hypertidy, r-spatial,
r-universe, individual maintainers, and CRAN — with no unified narrative
tying them together. A Python user looking at the landscape sees xarray's
documentation, Pangeo's tutorials, and Earthmover's marketing. An R user
looking at the same landscape sees a constellation of packages with
inconsistent documentation, varying levels of CRAN presence, and no obvious
entry point.

zaro's design documents (including this one) are a small step toward making
the unified capability visible. They explain not just what zaro does but
why it exists, what it shares with the Python stack, and where it fits in
the broader ecosystem. This kind of documentation — architecture decisions,
ecosystem positioning, design rationale — is what turns a collection of
packages into a coherent project.

## Concrete interoperability examples

These are real workflows where R and Python interoperate through shared
specification, not bridges:

**Zarr V3 round-trip.** Write a 4D climate dataset from xarray to Zarr V3
on S3. Read the same store with zaro in R. The metadata (zarr.json), codec
chain (zstd compression, endian byte ordering), chunk layout, and fill
values are all specified by the Zarr V3 standard. No conversion needed.

**Kerchunk/VirtualiZarr virtual datasets.** Generate Kerchunk references
for a collection of NetCDF files using VirtualiZarr in Python. Publish the
reference document (JSON or Parquet) alongside the original files. Read
the virtual dataset with zaro's ReferenceStore in R. The chunk references
(url, offset, length tuples) are a shared specification; both languages
resolve them identically.

**Arrow-mediated I/O.** Both PyArrow and the R arrow package use Arrow's
C++ FileSystem to access S3. The connection parameters (endpoint, region,
credentials) are the same. The byte-range read semantics are the same.
The compression codecs are the same C++ implementations.

**GDAL virtual filesystems.** Both rasterio and gdalraster use GDAL's
vsicurl for HTTP range requests, vsis3 for S3 access, and the GDAL Zarr
driver for reading Zarr stores. The configuration options, environment
variables (AWS_NO_SIGN_REQUEST, GDAL_HTTP_MAX_RETRY), and caching
behaviour are identical.

## Implications for zaro's design

zaro embodies these principles in its architecture:

1. **Specification-driven, not library-driven.** zaro reads Zarr V3 by
   implementing the specification, not by wrapping a Zarr library. The
   metadata parser reads JSON according to the Zarr V3 spec. The codec
   pipeline applies algorithms according to the spec. The chunk addressing
   follows the spec's key encoding rules.

2. **Shared infrastructure, not parallel infrastructure.** Arrow's C++
   codecs and filesystems are the same in R and Python. GDAL's VSI layer
   is the same in R and Python. zaro uses these shared foundations rather
   than building R-specific alternatives.

3. **Composable primitives.** zaro returns plain R arrays. It doesn't
   impose a data model, a coordinate system, a visualization framework,
   or a parallel computing strategy. Those are the job of packages above
   it in the stack (ndr, tidync, raadtools).

4. **Optional dependencies for optional capabilities.** Icechunk access
   (when needed) would come via an optional IcechunkStore backend. Blosc
   decompression comes via an optional blosc dependency. HTTP access comes
   via an optional gdalraster dependency. The core — JSON parsing, Arrow
   codecs, array assembly — stands alone.

These aren't novel principles. They're how Unix tools work, how the web
works, and how the best scientific software works. But they're worth stating
explicitly because the R geospatial community sometimes forgets to.
