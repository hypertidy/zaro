# Write sharded Zarr V3 arrays with zarr-python, plus the expected values,
# for zaro's interop test (test-sharding.R). Usage:
#   python3 zarr_python_sharded.py OUTDIR
# Each case writes OUTDIR/<name>.zarr and OUTDIR/<name>.bin (the full array,
# fill included, C order, little endian, the array's dtype). ASCII only.
import sys, os
import numpy as np
import zarr
from zarr.codecs import BytesCodec, Crc32cCodec, GzipCodec, ShardingCodec, ZstdCodec

out = sys.argv[1]
os.makedirs(out, exist_ok=True)

def case(name, data, shards, chunks, compressors, index_location="end",
         write=None, fill=0):
    path = os.path.join(out, name + ".zarr")
    a = zarr.create_array(store=path, shape=data.shape, dtype=data.dtype,
                          chunks=chunks, shards=shards, compressors=compressors,
                          fill_value=fill, zarr_format=3, overwrite=True,
                          dimension_names=[f"d{i}" for i in range(data.ndim)])
    if index_location != "end":
        # rewrite the codec with the index at the start
        a = zarr.create_array(store=path, shape=data.shape, dtype=data.dtype,
                              chunks=shards, zarr_format=3, overwrite=True,
                              fill_value=fill,
                              serializer=ShardingCodec(
                                  chunk_shape=chunks,
                                  codecs=[BytesCodec(), *compressors],
                                  index_codecs=[BytesCodec(), Crc32cCodec()],
                                  index_location="start"),
                              compressors=None)
    full = np.full(data.shape, fill, dtype=data.dtype)
    if write is None:
        a[...] = data
        full[...] = data
    else:
        for sl in write:
            a[sl] = data[sl]
            full[sl] = data[sl]
    full.astype(full.dtype.newbyteorder("<")).tofile(os.path.join(out, name + ".bin"))
    print(name, a.metadata.codecs[0].__class__.__name__, data.dtype, data.shape)

rng = np.random.default_rng(42)
f = rng.normal(size=(37, 50)).astype("float32")
case("f32_end_zstd", f, shards=(16, 20), chunks=(8, 5), compressors=[ZstdCodec(level=3)])

u = (np.arange(30 * 24 * 5) % 65000).astype("uint16").reshape(5, 24, 30)
case("u16_start_gzip", u, shards=(2, 12, 15), chunks=(1, 6, 5),
     compressors=[GzipCodec(level=5)], index_location="start")

# a compressor after sharding_indexed: the whole shard object is compressed
w = (np.arange(18 * 22) * 3).astype("int32").reshape(18, 22)
pw = os.path.join(out, "i32_whole_shard_zstd.zarr")
aw = zarr.create_array(store=pw, shape=w.shape, dtype=w.dtype, chunks=(9, 11),
                       zarr_format=3, overwrite=True, fill_value=0,
                       serializer=ShardingCodec(chunk_shape=(3, 11),
                                                codecs=[BytesCodec()],
                                                index_codecs=[BytesCodec(), Crc32cCodec()]),
                       compressors=[ZstdCodec(level=1)])
aw[...] = w
w.astype("<i4").tofile(os.path.join(out, "i32_whole_shard_zstd.bin"))
print("i32_whole_shard_zstd", [c.__class__.__name__ for c in aw.metadata.codecs])

# partial writes: two blocks written, the rest is missing shards or empty chunks
p = rng.integers(-1000, 1000, size=(40, 40)).astype("int32")
case("i32_partial", p, shards=(20, 20), chunks=(5, 5), compressors=[ZstdCodec()],
     write=[np.s_[0:7, 0:12], np.s_[25:40, 30:40]], fill=-9999)
