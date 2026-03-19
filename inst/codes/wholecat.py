import asyncio
import math
import numpy as np
import pyarrow as pa
import pyarrow.parquet as pq
from async_tiff import TIFF
from async_tiff.store import S3Store
from concurrent.futures import ThreadPoolExecutor

async def extract_one(store, path, time_idx):
    tiff = await TIFF.open(path, store=store)
    ifd = tiff.ifds[0]
    
    offsets = ifd.tile_offsets
    byte_counts = ifd.tile_byte_counts
    nx = math.ceil(ifd.image_width / ifd.tile_width)
    ny = math.ceil(ifd.image_height / ifd.tile_height)
    n = len(offsets)
    
    # Compute chunk indices from flat tile index
    y_chunks = [i // nx for i in range(n)]
    x_chunks = [i % nx for i in range(n)]
    
    return {
        "time_idx": [time_idx] * n,
        "y_chunk": y_chunks,
        "x_chunk": x_chunks,
        "path": [path] * n,
        "offset": list(offsets),
        "length": list(byte_counts),
    }

async def build_references(file_list):
    store = S3Store(
        "us-west-2.opendata.source.coop",
        region="us-west-2",
        skip_signature=True,
    )
    
    # Process files concurrently
    tasks = [
        extract_one(store, path, i)
        for i, path in enumerate(file_list)
    ]
    results = await asyncio.gather(*tasks)
    
    # Combine into single Arrow table
    combined = {k: [] for k in results[0]}
    for r in results:
        for k, v in r.items():
            combined[k].extend(v)
    
    table = pa.table({
        "time_idx": pa.array(combined["time_idx"], type=pa.uint16()),
        "y_chunk":  pa.array(combined["y_chunk"],  type=pa.uint16()),
        "x_chunk":  pa.array(combined["x_chunk"],  type=pa.uint16()),
        "path":     pa.array(combined["path"],      type=pa.string()),
        "offset":   pa.array(combined["offset"],    type=pa.uint64()),
        "length":   pa.array(combined["length"],    type=pa.uint32()),
    })
    
    pq.write_table(table, "ghrsst_refs.parquet")
    print(f"Wrote {len(table)} references")

asyncio.run(build_references(file_list))
