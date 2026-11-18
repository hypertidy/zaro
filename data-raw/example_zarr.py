# Write inst/extdata/example.zarr, the small local store the help page
# examples read. Run from the package root:
#
#   python3 data-raw/example_zarr.py
#
# Written with zarr-python 3.2.1 and numpy 2.5.1. A Zarr V3 group with
# consolidated metadata:
#
#   /           attributes: title
#   lon, lat    coordinates (float64; 16 and 8 values)
#   time        coordinate (int32, days since 2000-01-01; 3 values)
#   temp        float32 (time, lat, lon), chunks (1, 4, 8), gzip, NaN fill,
#               a few values missing
#   detail/     a group
#   detail/elev int16 (y, x) 16 x 16, sharded: shards (8, 8) of (4, 4)
#               chunks, gzip
#
# Values are deterministic (no random numbers), so the store is the same
# every time it is written.

import os
import shutil

import numpy as np
import zarr

path = os.path.join("inst", "extdata", "example.zarr")
if os.path.exists(path):
    shutil.rmtree(path)

gz = [zarr.codecs.GzipCodec(level=6)]
root = zarr.open_group(path, mode="w", zarr_format=3)
root.attrs["title"] = "zaro example store"

lon = np.linspace(140.25, 147.75, 16)
lat = np.linspace(-38.25, -41.75, 8)
time = np.array([0, 1, 2], dtype="int32")

def coord(name, values, attrs):
    a = root.create_array(name, shape=values.shape, chunks=values.shape,
                          dtype=values.dtype, compressors=gz,
                          dimension_names=[name])
    a[:] = values
    a.attrs.update(attrs)

coord("lon", lon, {"units": "degrees_east"})
coord("lat", lat, {"units": "degrees_north"})
coord("time", time, {"units": "days since 2000-01-01"})

t, y, x = np.meshgrid(time, lat, lon, indexing="ij")
temp = (15 + 0.5 * (y + 40) + 0.2 * (x - 144) + t).astype("float32")
temp[0, 0, :3] = np.nan
a = root.create_array("temp", shape=temp.shape, chunks=(1, 4, 8),
                      dtype="float32", compressors=gz, fill_value=np.nan,
                      dimension_names=["time", "lat", "lon"])
a[:] = temp
a.attrs.update({"units": "degC", "long_name": "temperature"})

g = root.create_group("detail")
g.attrs["description"] = "a nested group with a sharded array"
yy, xx = np.meshgrid(np.arange(16), np.arange(16), indexing="ij")
elev = (100 * yy + xx).astype("int16")
e = g.create_array("elev", shape=elev.shape, shards=(8, 8), chunks=(4, 4),
                   dtype="int16", compressors=gz, fill_value=-1,
                   dimension_names=["y", "x"])
e[:] = elev
e.attrs["units"] = "m"

zarr.consolidate_metadata(path)
