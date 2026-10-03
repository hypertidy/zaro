# Track extraction from the BRAN2023 virtual Zarr

Files:

- track_extract.R: helpers. zaro opens the VirtualiZarr Parquet store and
  decodes chunks; altarr presents `temp` as a lazy 4D R array
  (xt_ocean, yt_ocean, st_ocean, Time); a batched fetch resolves all
  chunks a subset needs from the manifest and issues the byte-range GETs
  through one curl pool capped at 8 connections.
- run_track.R: a 30 day made-up track off eastern Tasmania (one point per
  day, depth 5 to 200 m), extracted with one `x[cbind(i, j, k, l)]`.
- mock_thredds.py: stand-in data server for dry runs (synthetic chunks
  encoding their own index, 0.25 s latency, records peak concurrency).

Run for real (machine that can reach thredds.nci.org.au):

    Rscript run_track.R            # writes track_real.csv

Dry run:

    python3 mock_thredds.py 8765 0.25 &
    NO_PROXY=127.0.0.1 Rscript run_track.R mock 8765

`TRACK_MAX_CON` (default 8) sets the connection cap.

Dry-run results (2026-10-03, refs and shards from GitHub, data from mock):

| max_con | requests | bytes   | fetch time | peak concurrency | values correct |
|---------|----------|---------|------------|------------------|----------------|
| 8       | 30       | 1.95 MB | 1.28 s     | 8                | yes            |
| 1       | 30       | 1.95 MB | 7.62 s     | 1                | yes            |

Each track point is its own 300x300 chunk (~65 KB compressed), so a track
costs one THREDDS request per point per day/level. Metadata, coordinates
and one manifest shard (~1.2 MB, covers ~32 days of temp) come from
GitHub, not THREDDS.
