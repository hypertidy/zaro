# zaro and byterefs

_November 2026_

## The stance

byterefs (<https://github.com/byterefs/spec>) specifies a byte-reference
table: for every chunk of an array, which file holds it, at what byte
range, and what a reader needs to decode it. zaro's roadmap
(`007-roadmap.md`) arrived at the same idea from the reading side:
"everything is a refs table". These are one thing, and zaro should treat
byterefs as its table, not keep a private variant.

So:

1. **zaro reads byterefs** in every encoding the spec publishes: the
   SQLite store now, the Parquet export once it is fixed (brief question 7).
2. **zaro's plan rows are byterefs view rows.** `zaro_plan()` already
   returns, per chunk, where the bytes are; it uses byterefs column names,
   with zaro-only columns (such as `transport`) as extras.
3. **zaro can produce byterefs** for anything it can open (native Zarr V2
   and V3, sharded V3, kerchunk, VirtualiZarr), by writing those rows plus
   the array description. raad.virtualize produces byterefs from a GDAL
   scan of NetCDF/HDF5/GeoTIFF sources; zaro would cover Zarr-shaped
   sources, without GDAL.
4. **Spec questions zaro runs into go to byterefs/spec as issues.**
   Workarounds don't stay inside zaro.

The enriched-Parquet design in zaro's `DESIGN.md` predates byterefs and is
superseded by it.

## The two tables side by side

| byterefs view (`refs_<array>`) | zaro plan (0.0.1.9022) | Notes |
|---|---|---|
| `dim_0 .. dim_{n-1}` | `dim_0 .. dim_{n-1}` | 0-based, C order in both |
| `present` | `present` (logical) | byterefs `present = 0` is known empty, read as fill |
| `path` | `path` | byterefs: remapped for the chosen target |
| `offset` | `offset` | |
| `size` | `size` | zaro `NA` = whole object of unknown size (native Zarr) |
| `info` | (codecs from array metadata) | block-info text; zaro decodes from the array's codec chain |
| `data` | `data` | inline chunk bytes |
| (none) | `key` | zaro only: the Zarr key, kept for store reads |
| (none) | `transport` | zaro only: how the bytes are fetched (`http`, `store`, ...) |

Until 0.0.1.9022 the plan used `url`, `length`, `inline` and a list column
`index`.

Native Zarr chunks are whole objects whose size isn't known until fetched.
byterefs rows always carry a size, so a zaro-produced table either looks
sizes up (one HEAD or listing per object) or the spec allows an unknown
size. That is a question for byterefs, below.

## Changes, in order

1. **Done** (0.0.1.9018). The SQLite reader applies `meta.default_target`
   (decision 0004), accepts exactly `byterefs/0.1` and `mdim-refs/0.2`
   (decision 0003), and opens `byterefs://` (`mdimrefs://` stays as an
   alias).
2. **Done** (0.0.1.9022). Plan columns renamed to byterefs names: `url`
   to `path`, `length` to `size`, `inline` to `data`, `present` and integer
   `dim_i` columns in place of `index`, in byterefs view order; `key` and
   `transport` kept as extras.
3. **Read byterefs natively.** Today a byterefs store is served through
   synthesized Zarr V2 keys and one view query per key. Instead,
   `store_refs()` takes all of a plan's keys in one query, and uses the
   placement-first lookup the spec describes ("One chunk by global key"),
   not a filter on the view, which the spec measures at 0.9 to 1.9 s per
   chunk at about 1M chunks.
4. **Read the Parquet export** (`refs_<array>.parquet`, `arrays.json`,
   `sources.parquet`) with arrow, following the layout byterefs settles
   on.
5. **Write byterefs**: `zaro_plan()` rows plus the array description to the
   published encoding, so zaro can virtualize Zarr-shaped stores into the
   same table form as everything else.

## Questions for byterefs

From zaro's experience reading real stores, raised on byterefs/spec on
2026-11-08:

1. **The fast chunk lookup is specified but not implemented**
   ([byterefs/spec#8](https://github.com/byterefs/spec/issues/8)). The spec
   names a placement-first `mr_chunk_sql()`; raad.virtualize has
   `mr_chunk()`, which filters the view (the slow path). Either implement
   it in the reference code or mark the text as proposed.
2. **Codec description (brief question 1)**
   ([#9](https://github.com/byterefs/spec/issues/9)). zaro, like wxy-refs, converts
   Zarr V2 codec ids to a V3 codec chain on read. V3-only codecs (`crc32c`,
   V3 blosc configuration, byte order as the `bytes` codec) have no V2 id.
   Proposal: describe codecs as a V3 chain in encode order, with V2 ids
   accepted during 0.x.
3. **Sharded Zarr as a source**
   ([#10](https://github.com/byterefs/spec/issues/10)). A sharded V3 array's inner chunks map one
   to one onto rows (the shard object as `path`, the inner chunk's range as
   `offset`/`size`) when nothing but `crc32c` follows `sharding_indexed`.
   A compressor on the whole shard can't be virtualized; it belongs with
   brief question 13 (storage that is not chunked).
4. **JSON numbers must round-trip doubles**
   ([#11](https://github.com/byterefs/spec/issues/11)). zaro (consolidated metadata)
   and raad.virtualize (the R-built BRAN index) both truncated BRAN's
   `scale_factor` 0.007782219909131527 to 0.0078 through jsonlite's default
   of 4 significant digits, independently. A rule (JSON numbers carry 17
   significant digits, or enough to round-trip) and a fixture with that
   value would catch it.
5. **Unknown sizes**
   ([#12](https://github.com/byterefs/spec/issues/12)). Native Zarr chunks have no size without a request.
   Either a byterefs row may leave `size` NULL for a whole object, or a
   producer must measure it. Worth deciding before zaro writes tables.
6. **IMPLEMENTATIONS.md**
   ([byterefs/spec#13](https://github.com/byterefs/spec/pull/13)) says no implementation writes `byterefs/0.1`;
   raad.virtualize does, and zaro now reads it (and applies the default
   target).
