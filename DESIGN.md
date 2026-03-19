# Enriched Parquet Refs: Design & Integration
#
# A self-contained reference store format for cloud-optimised geospatial arrays.
# Single parquet file = byte-range catalogue + spatial index + codec spec + metadata.

## Format Specification (v0.1)

### Columns (per chunk/tile)

| Column     | Type      | Description                                    |
|------------|-----------|------------------------------------------------|
| path       | string    | Source file URL (http/s3/local)                |
| offset     | int64     | Byte offset in source file                     |
| length     | int64     | Compressed chunk size in bytes                 |
| tile_col   | int32     | 0-based tile column index                      |
| tile_row   | int32     | 0-based tile row index                         |
| bbox_xmin  | float64   | Tile west edge (computed from geotransform)    |
| bbox_xmax  | float64   | Tile east edge                                 |
| bbox_ymin  | float64   | Tile south edge                                |
| bbox_ymax  | float64   | Tile north edge                                |
| time       | timestamp | Materialized from dimension coordinates        |
| ifd        | int32     | IFD index within TIFF (0 for single-image)     |

Note: image_w, image_h, tile_w, tile_h, dtype, compression, bits_per_sample,
samples_per_pixel, crs_epsg from rustycogs are CONSTANT per array and move
to file-level metadata (not repeated per row). This is important: the raw
rustycogs output repeats these ~2500 times per file × 8400 files = 21M rows
of redundant strings. Moving them to metadata shrinks the parquet significantly.

### File-level metadata (parquet key-value pairs)

| Key               | Value                                                |
|-------------------|------------------------------------------------------|
| format_version    | "enriched-parquet-refs/0.1"                          |
| variable          | Variable name ("analysed_sst")                       |
| geotransform      | "x_origin,x_res,x_skew,y_origin,y_skew,y_res"       |
| crs_wkt           | WKT2 or "EPSG:NNNN"                                  |
| zarray            | Full .zarray JSON (dtype, chunks, compressor, filters)|
| zattrs            | Full .zattrs JSON (scale, offset, units, fill_value)  |
| image_width       | Source image width in pixels                          |
| image_height      | Source image height in pixels                         |
| tile_width        | Tile width in pixels                                  |
| tile_height       | Tile height in pixels                                 |
| dtype             | NumPy dtype string ("<i2", "<f4", etc.)               |
| compression       | Compressor name ("ZSTD", "DEFLATE", etc.)             |
| predictor         | TIFF predictor (0=none, 2=horizontal, 3=float)        |
| bits_per_sample   | Bits per sample                                       |
| samples_per_pixel | Samples per pixel (bands within tile)                 |
| n_files           | Number of source files                                |
| dim_time          | Comma-separated ISO timestamps (dimension coords)     |
| dim_lat           | (optional) latitude coordinate array                  |
| dim_lon           | (optional) longitude coordinate array                 |


## Provenance: what can produce this format?

### 1. rustycogs (primary path)
   tiff_refs() → enrich_refs() → write_parquet()
   Already produces the raw columns. Enrichment adds bbox, time, metadata.

### 2. From existing kerchunk parquet (harvesting)
   arrow::read_parquet("refs.0.parq") gives (path, offset, size).
   Add tile indices from row order (implicit in kerchunk).
   Reconstruct from .zmetadata sidecar.

### 3. From VirtualiZarr parquet manifests
   Same as #2 but read from the refs.N.parq files per variable.

### 4. From DMR++ (OPeNDAP)
   Parse the DMR++ XML to extract chunk byte ranges.
   Requires knowing the on-disk layout (typically NetCDF4/HDF5).

### 5. From icechunk
   Export chunk refs from icechunk's internal store.
   (Not straightforward today, but the data is there.)


## Consumers

### A. zaro (R, Arrow-native)

The primary consumer. EnrichedParquetStore opens the parquet, exposes
zaro_meta() from embedded metadata, resolves chunks by tile index or
spatial bbox query. The decode pipeline is:

  Arrow predicate pushdown (spatial/temporal filter)
  → fetch_bytes (curl/arrow-s3)
  → Arrow codec decompress
  → readBin (type coercion)
  → cumsum per row (predictor reversal, 3 lines of R)
  → scale/offset (CF convention)

No GDAL in the pixel path. No zarr driver. No codec registry.
The parquet IS the store, Arrow IS the codec engine.


### B. GDAL via /vsikerchunk/ (existing path)

GDAL already reads kerchunk parquet via /vsikerchunk/.
The enriched format is a superset — extra columns are ignored,
(path, offset, length) are in the same positions.

To fully leverage the enriched metadata, GDAL would need to:

1. Read parquet file-level KV metadata for the .zarray/.zattrs
   (instead of requiring sidecar JSON files)
2. Apply the tiff_predictor filter (our earlier PR)
3. Expose the geotransform and CRS from parquet metadata

This is ~3 changes in vsikerchunk_parquet_ref.cpp:
- In the metadata synthesis step, check parquet KV pairs first
- Fall through to sidecar .zarray/.zattrs if not found
- Pass geotransform/CRS through to the zarr driver's dataset

The beauty is backward compatibility: files without KV metadata
still work via the sidecar JSON path. Files WITH KV metadata
skip the sidecar reads (fewer HTTP requests for remote stores).


### C. Any Arrow-capable language

The format is just parquet. Python reads it with pyarrow.
Julia reads it with Arrow.jl. Rust reads it with arrow-rs.
The spatial subsetting is column predicates — no geospatial
library needed for the index query, only for the pixel decode.

```python
# Python example
import pyarrow.parquet as pq
t = pq.read_table("ghrsst_enriched.parquet")

# Spatial subset — pure Arrow
tiles = t.filter(
    (t["bbox_xmin"] < 170) & (t["bbox_xmax"] > 140) &
    (t["bbox_ymin"] < -30) & (t["bbox_ymax"] > -50)
)

# Metadata
md = pq.read_schema("ghrsst_enriched.parquet").metadata
zarray = json.loads(md[b"zarray"])
```


## Relationship to existing hypertidy packages

### grout
  Tile-to-bbox mapping is exactly what grout does.
  enrich_refs() effectively materialises grout::grout() output
  as parquet columns. For the enriched parquet, grout's logic
  is applied once at write time rather than at every read.

### vaster
  Grid assembly from tiles. read_window() in the zaro consumer
  is doing what vaster does — placing tile matrices into an
  output grid. The enriched parquet metadata (geotransform,
  image dimensions, tile dimensions) is exactly vaster's input.

### vapour
  GDAL read path. vapour::vapour_read_raster() is the validation
  baseline — pixel-exact comparison against GDAL's direct read.
  The enriched parquet path should produce identical values.

### ndr
  Higher-level array interface. The enriched parquet store would
  be a backend for ndr — ndr provides the xarray-like API,
  the enriched parquet provides the chunk resolution and decode.

### tissot / geographiclib
  Projection analysis. If warping is needed, it happens AFTER
  tile assembly, through GDAL's warper or native R reprojection.


## Why not just use zarr?

Because zarr conflates storage layout with codec specification.

When you virtualise TIFF tiles as zarr chunks, you inherit zarr's
assumption that the codec chain is self-describing within the zarr
spec. But TIFF predictor isn't a zarr codec. JPEG quantization
tables aren't zarr metadata. The format-specific transform lives
in the gap between "where are the bytes" and "how to decode them."

The enriched parquet separates these concerns:
- Columns answer "where are the bytes" (path, offset, length)
- Spatial columns answer "which bytes do I need" (bbox, time)
- File metadata answers "how to decode them" (zarray with full
  filter chain including format-specific transforms)

The zarr JSON is embedded AS metadata, not used AS the store format.
This means the codec spec can include non-standard filters
(tiff_predictor) without breaking any zarr implementation —
because the parquet isn't claiming to be a zarr store.

It CAN be consumed as zarr (via /vsikerchunk/ or zaro's
ReferenceStore), but it doesn't HAVE to be.


## Next steps

1. [ ] Validate enrich_refs() against actual rustycogs output from GHRSST
2. [ ] Test spatial subsetting performance with Arrow predicate pushdown
       on the full 21.5M row parquet
3. [ ] Implement zaro EnrichedParquetStore and verify pixel-exact decode
       against vapour::vapour_read_raster() baseline
4. [ ] Benchmark: enriched parquet → zaro vs. direct GDAL /vsicurl/ read
5. [ ] Prototype GDAL integration: parquet KV metadata → .zarray synthesis
       in vsikerchunk_parquet_ref.cpp
6. [ ] Test round-trip: rustycogs → enriched parquet → GDAL /vsikerchunk/
       with tiff_predictor filter → warped output
7. [ ] Explore partitioned parquet for very large collections
       (partition by year/month for efficient temporal subsetting)
8. [ ] Write harvester functions for kerchunk/VirtualiZarr/DMR++ → enriched
