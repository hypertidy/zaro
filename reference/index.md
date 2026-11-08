# Package index

## Open and explore

Connect to a store and see what is in it, without reading data.

- [`zaro()`](https://hypertidy.github.io/zaro/reference/zaro.md) : Open
  a Zarr store
- [`zaro_dataset()`](https://hypertidy.github.io/zaro/reference/zaro_dataset.md)
  : Open a store as a dataset: every array, its dimensions, no data read
- [`zaro_list()`](https://hypertidy.github.io/zaro/reference/zaro_list.md)
  : List contents of a Zarr store
- [`zaro_meta()`](https://hypertidy.github.io/zaro/reference/zaro_meta.md)
  : Get metadata for a Zarr node

## Read

Hyperslabs as R arrays, or lazy arrays that read on demand.

- [`zaro_read()`](https://hypertidy.github.io/zaro/reference/zaro_read.md)
  : Read data from a Zarr array
- [`zaro_array()`](https://hypertidy.github.io/zaro/reference/zaro_array.md)
  : Open a Zarr array as a lazy R array

## Plan and fetch

Which bytes a read needs (a byterefs-shaped table), and fetching them.

- [`zaro_plan()`](https://hypertidy.github.io/zaro/reference/zaro_plan.md)
  : Plan a read: the byte references a selection needs, without reading
  them
- [`zaro_fetch()`](https://hypertidy.github.io/zaro/reference/zaro_fetch.md)
  : Fetch the bytes of a plan

## Chunks

Work with individual chunks.

- [`zaro_chunk()`](https://hypertidy.github.io/zaro/reference/zaro_chunk.md)
  : Read a single chunk by grid index
- [`zaro_chunks()`](https://hypertidy.github.io/zaro/reference/zaro_chunks.md)
  : Read multiple chunks by index range
- [`zaro_chunk_info()`](https://hypertidy.github.io/zaro/reference/zaro_chunk_info.md)
  : Get chunk grid information for an array
- [`zaro_chunk_apply()`](https://hypertidy.github.io/zaro/reference/zaro_chunk_apply.md)
  : Apply a function over chunks

## Package

- [`zaro-package`](https://hypertidy.github.io/zaro/reference/zaro-package.md)
  : zaro: Zarr via Arrow
