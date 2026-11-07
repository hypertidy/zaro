# zaro roadmap: from working reader to daily driver

_November 2026_

## Where zaro is

zaro reads real cloud Zarr end to end: V2 and V3 metadata, consolidated
metadata, Arrow codecs, blosc, S3/GCS/HTTP/local, and five ways to address
virtual data (kerchunk JSON, kerchunk Parquet, VirtualiZarr Parquet,
mdim-refs SQLite, enriched Parquet sketches). `zaro_array()` hands a store
to altarr as a lazy base-R array, and the track-extract demo showed that one
batched curl pool beats serial chunk fetches about 6x.

What's missing for daily use:

- **Common V3 encodings.** No `sharding_indexed`, `crc32c`, `vlen-utf8`, or
  numcodecs filters (`delta`, `fixedscaleoffset`, `bitround`). New data such
  as GeoZarr, EOPF Sentinel, Icechunk-backed stores and most V3 writers use
  sharding, so zaro can't read much of what gets published from now on.
- **Batched I/O.** The fast fetch path is in `inst/examples`, not in the package.
  `zaro_read()` still goes one chunk per request (or one mirai task per
  chunk).
- **A link to ndr.** ndr reads through GDAL7 and altarr. zaro already makes
  altarr arrays, but nothing connects the two.
- **Ways to find data.** Users have to know a store URL and its quirks
  (region, anonymous access, `virtualizarr://` prefix).
- **Quiet output by default.** Verbose messages are good for discovery but
  get in the way in scripts.

## The thesis

> **zaro gets the bytes; ndr does the meaning.**

zaro's job: take *any address* (URL, catalogue name, or refs table), work
out *which bytes* a selection needs, *fetch them efficiently*, and *decode*
them into plain R arrays. It doesn't do coordinates, time decoding or `sel()`.
Those belong to ndr.

The idea that ties this together is: **everything is a refs table.**

Every store backend zaro has, native or virtual, comes down to the same
thing: `chunk key -> (url, offset, length)`, or an inline value. A native
Zarr chunk is the row `(store_url/key, 0, NA)`. A kerchunk or VirtualiZarr
or mdim-refs chunk is a byte range in a NetCDF or TIFF. A shard is a byte
range inside a shard object. So the core pipeline becomes:

```
address --resolve--> store --plan(selection)--> refs table --fetch--> raw --decode--> array
```

Each stage can be inspected and reused separately:

- `zaro_plan()` returns the refs table *before* any data is read. It gives
  the cost of a read (requests, bytes, hosts), and you can save it, share
  it, or pass it to another tool (GDAL `/vsikerchunk/`, Python, a job
  scheduler).
- One fetcher (a curl multi pool with a per-host concurrency cap, ranged
  GETs and merging of adjacent ranges) serves every backend. Arrow S3/GCS
  stays as the authenticated path.
- One decoder (the codec chain) serves every backend.

Nothing else in the R ecosystem does this, and it's what separates zaro from
the CRAN `zarr` package, pizzarr, and the GDAL Zarr driver. Those are
format readers. zaro would be a byte planner and fetcher for array data.

## The zaro <-> ndr contract

Keep it small, written down, and tested on both sides:

```
zaro_dataset(source) -> list of arrays, each with:
  name, dims (R order), shape, chunk_shape, dtype, fill_value, attrs,
  fetch(chunk_index_matrix) -> list of decoded chunks   # batched
```

That's almost exactly what `zaro_array()` already gives altarr.
`ndr::open_dataset(src, backend = "zaro")` then builds its coordinates from
the 1D arrays and its lazy variables from the rest. GDAL7 stays the backend
for NetCDF/HDF5/GRIB; zaro becomes the GDAL-free backend for Zarr and
virtual stores.

The dividing line on decoding:

- **zaro**: storage encoding. Codecs, `fill_value`, and optionally CF packing
  (`scale_factor`, `add_offset`, `_FillValue`), because packing is storage
  encoding, and readers get it wrong all the time.
- **ndr**: coordinates, CF time, labels, selection, broadcasting, reductions.

## Plan

Each phase ends with something usable, not just internals.

### Phase 1: zaro inside ndr (first, about a week)

The quickest way to put zaro into daily use is to make it disappear behind
ndr.

1. Add `zaro_dataset(store)`: classify arrays (coordinate or data variable)
   from consolidated metadata, and return per-array `zaro_array()` handles
   plus attributes.
2. Add a zaro backend to `ndr::open_dataset()` (dispatch on `zaro://`, a
   `.zarr` suffix, `virtualizarr://`, `mdimrefs://`, or an existing zaro
   store object).
3. Acceptance: these run in ndr with no GDAL in the pixel path, and give
   pixel-identical results to the GDAL7 backend:
   - AODN GSLA: `open_dataset(...)$GSLA |> sel(TIME = "2024-01-01")`
   - BRAN2023 temp (virtual): the track extraction from `inst/examples`
   - one V2 CMIP6 Pangeo store
4. Add `options(zaro.verbose = FALSE)` and make quiet the default once
   inside ndr.

### Phase 2: planned, batched I/O in core

1. `zaro_plan(store, path, start, count)` returns a refs table tibble/data
   frame: `key, url, offset, length, inline`, plus a cost summary in print.
   Every store class implements `store_refs(keys)`. Native stores give
   whole-object rows.
2. `zaro_fetch(plan)`: move the curl pool from `inst/examples/track-extract`
   into the package. Per-host cap, retries with backoff, range merging,
   `If-Match`/ETag where available. Arrow S3 stays for signed access.
3. `zaro_read()` and the altarr fetch both go through plan + fetch. mirai
   remains for *decode*-bound work on big reads, not for I/O.
4. Benchmarks to commit to the repo: chunks/s and MB/s against AODN S3,
   NCI THREDDS, and GitHub-hosted refs, compared with GDAL `/vsicurl/` and
   xarray + fsspec on the same machine.

### Phase 3: read what's published now (V3 completeness)

1. `sharding_indexed`: read the shard index (end or start, `crc32c`
   checked), then plan inner chunks as byte ranges inside the shard. This
   fits the refs-table model directly, because a shard is just another
   level of byte ranges.
2. Codecs: `crc32c`, `vlen-utf8`/`vlen-bytes`, `transpose` in
   `zaro_array()`, numcodecs `delta`, `fixedscaleoffset`, `bitround`,
   `quantize`, plus `zlib`/`gzip` V3 configs.
3. Get the zarr-conformance suite in CI green for everything read-side.
4. Acceptance: read an EOPF Sentinel-2 Zarr product and a GeoZarr sample.

### Phase 4: finding data (making it a habit)

1. Build the URI interpreter (`006-uri-interpreter.md`): `zaro_uri()`
   normalizes HTTPS cloud URLs to `s3://`/`gs://`, strips viewer
   parameters, picks the region, sets `anonymous`.
2. A small catalogue: `zaro_catalog()` / `zaro("bran2023/temp")`. It's a
   versioned Parquet/JSON registry (name, address, auth, notes) hosted in
   a repo, seeded with the stores we already use: AODN cloud-optimised,
   BRAN2023 virtual refs, GHRSST refs, OISST, NSIDC sea ice, Copernicus
   ARCO, Pangeo CMIP6.
3. sooty integration: where sooty already lists the files of a collection,
   generate refs for it (mdim-refs or enriched Parquet), so a whole
   collection opens as one array in zaro and ndr.

### Phase 5: release

- CRAN submission once phases 1-3 are done. Remove `Remotes:` by keeping
  altarr in Suggests and loading it conditionally, or by getting altarr to
  CRAN first. Get `gdalraster` and `blosc` fully optional.
- pkgdown site with three worked articles that match the acceptance
  workloads.
- A short paper or blog post: "everything is a refs table".

### Later / speculative

- **Icechunk read.** Manifests and snapshots are flatbuffers. Either read
  them directly, or export Icechunk to a refs table (see `003`). With the
  refs-table core in place, the second option is cheap.
- **Writing refs, not data.** `zaro_refs_write(plan, format =
  c("kerchunk-parquet", "mdimrefs", "enriched"))`. This makes zaro a
  virtualizer as well as a reader: refs from GDAL multidim chunk info or
  from rustycogs, saved as a catalogue entry.
- **Writing Zarr.** Only if ndr needs it for results. Keep it V3,
  uncompressed or zstd, unsharded. Most write needs are served better by
  GDAL.
- **Contribute an Arrow store** to the CRAN `zarr` package, so the two
  packages complement each other.

## What to do first

Phase 1, step 1 to 3. It's small, mostly composes things that already
exist (`zaro_meta`, `zaro_array`, altarr, ndr's lazy layer), and turns
"zaro is a nice reader" into "I opened BRAN in ndr this morning without
GDAL". Phase 2 is the next most valuable piece, because it's where zaro
becomes faster than the alternatives and not just equivalent.
