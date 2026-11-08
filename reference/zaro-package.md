# zaro: Zarr via Arrow

Low-level Zarr interface (V2 and V3) built on Arrow filesystem and codec
infrastructure. Provides direct access to Zarr stores on local
filesystems, S3, and GCS via Arrow, with fallback to GDAL virtual
filesystems for HTTP and other protocols via gdalraster. Supports
Kerchunk JSON and Parquet reference stores for virtual Zarr access to
existing archives. Automatically detects Zarr version and handles S3
region redirects.

## Author

**Maintainer**: Michael Sumner <mdsumner@gmail.com>
([ORCID](https://orcid.org/0000-0002-2471-7511))

Authors:

- Michael Sumner <mdsumner@gmail.com>
  ([ORCID](https://orcid.org/0000-0002-2471-7511))

Other contributors:

- Hugo Gruson ([ORCID](https://orcid.org/0000-0002-4094-1476))
  \[contributor\]
