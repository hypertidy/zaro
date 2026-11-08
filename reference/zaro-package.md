# zaro: Zarr via Arrow

A low-level reader for Zarr arrays (V2 and V3, including sharded V3),
native or virtual, from local disk, HTTP(S), S3 and GCS. Virtual stores
read the bytes of existing NetCDF, HDF5 and GeoTIFF archives in place,
from Kerchunk JSON and Parquet references, VirtualiZarr manifests and
byterefs tables (SQLite, or the flat Parquet export read by row group).
Every read is a plan of byte references (one row per chunk) fetched
through one curl connection pool, then decoded in R. Zarr arrays can
also be opened as lazy R arrays (via altarr) with CF unpacking. Arrow
and GDAL (via gdalraster) are optional, for S3 and GCS listing and
credentials and for some codecs.

## See also

Useful links:

- <https://hypertidy.github.io/zaro/>

- <https://github.com/hypertidy/zaro>

- Report bugs at <https://github.com/hypertidy/zaro/issues>

## Author

**Maintainer**: Michael Sumner <mdsumner@gmail.com>
([ORCID](https://orcid.org/0000-0002-2471-7511))

Authors:

- Michael Sumner <mdsumner@gmail.com>
  ([ORCID](https://orcid.org/0000-0002-2471-7511))

Other contributors:

- Hugo Gruson ([ORCID](https://orcid.org/0000-0002-4094-1476))
  \[contributor\]
