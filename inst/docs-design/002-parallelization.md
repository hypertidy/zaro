# zaro: Parallelization Roadmap

_February 2026_

This document records the analysis of parallelization strategies for zaro,
following the initial package design. It captures the state of the R parallel
computing ecosystem at time of writing (mirai 2.6.0, futurize 0.1.0) and the
design decisions for how zaro should integrate with it.

## The parallelization opportunity

Chunk fetching and decoding in zaro is embarrassingly parallel. Each
`store_get()` + `decode_chunk()` call is independent — no shared state, no
ordering requirements, no communication between tasks. The results are
assembled into the output array only after all chunks are collected.

A `zaro_read()` call that spans many chunks (common for large hyperslabs
from cloud stores) is dominated by I/O latency. Sequential chunk fetching
over S3 or HTTP means each chunk waits for the previous one to complete.
Parallel fetching turns wall time from `n_chunks * latency` into roughly
`latency + (n_chunks / n_workers) * transfer_time`.

This is the ideal case for both mirai and the future/futurize ecosystem.

## mirai (2.6.0)

mirai is now R's convergence point for async and parallel computing. It is
the recommended async backend for Shiny, the only async backend for plumber2,
the parallel backend for purrr, and an official alternative communications
backend for R's `parallel` package.

### Why mirai fits zaro's chunk-fetch workload

**C-level dispatcher.** Per-task dispatch overhead is in the tens of
microseconds. This matters because individual chunk operations may be fast
(a small HTTP range request + zstd decompress might be 10-50ms), so framework
overhead needs to be negligible relative to task duration. Existing R
parallelism solutions typically operate in the millisecond range.

**`mirai_map()` over chunk indices.** The chunk fetch loop in `zaro_read()`
maps directly to `mirai_map()` — a list of chunk indices in, a list of
decoded chunk buffers out.

**`race_mirai()` for streaming assembly.** For cloud stores with variable
latency, you could start assembling the output array as chunks land rather
than waiting for the slowest one. This is the "process as completed" pattern
that race_mirai() enables. In practice this means the output array fills in
progressively rather than blocking until all chunks are ready.

**Persistent daemon connections.** mirai's daemon model means you set up
persistent workers that can hold open connections to the store. For S3/GCS
where connection setup has real cost (TLS handshake, credential resolution),
reusing connections across chunk fetches within a single `zaro_read()` call
is a meaningful performance gain.

**Deployment flexibility.** The same `daemons()` call scales from local
cores to SSH remotes, HPC clusters (Slurm, SGE, PBS), and now HTTP APIs
including Posit Workbench. For AAD workflows this means a `zaro_read()` call
that fetches chunks locally during development can fan out across Slurm
nodes in production with a one-line change.

### Sketch: mirai-native chunk fetching

```r
# direct mirai usage in zaro_read() internals
daemons(n_workers)

results <- mirai_map(chunk_indices, \(cidx) {
  raw <- fetch_chunk(store, path, cidx, meta)
  if (is.null(raw)) return(NULL)
  list(idx = cidx, values = decode_chunk(raw, meta))
}, store = store, path = path, meta = meta)[]

daemons(0)
```

Or with the race pattern for progressive assembly:

```r
mirais <- lapply(chunk_indices, \(cidx) {
  mirai(
    {
      raw <- fetch_chunk(store, path, cidx, meta)
      if (is.null(raw)) return(NULL)
      list(idx = cidx, values = decode_chunk(raw, meta))
    },
    store = store, path = path, cidx = cidx, meta = meta
  )
})

remaining <- mirais
while (length(remaining) > 0) {
  i <- race_mirai(remaining)
  result <- remaining[[i]][]
  if (!is.null(result)) {
    # insert into output array immediately
    out <- assemble_chunk(out, result, meta, start)
  }
  remaining <- remaining[-i]
}
```

## futurize (0.1.0)

futurize is the "magic touch" parallelizer from the Futureverse. It works as
a transpiler: capture a standard R expression (`lapply()`, `map()`,
`foreach()`), convert it to its parallel equivalent (`future_lapply()`,
`future_map()`, etc.), and execute. The user writes sequential code and pipes
it into `futurize()`.

### Why futurize fits zaro's design philosophy

**Separate "what" from "how".** zaro genuinely doesn't care how chunks are
fetched in parallel — it just needs them fetched. futurize embodies this
principle: the analysis code (chunk iteration) is independent of the
parallelization backend.

**Backend independence.** Because futurize is built on the future ecosystem,
`zaro_read()` automatically works with any future backend: mirai
(`future.mirai::mirai_multisession`), local multicore
(`future::multisession`), Slurm (`future.batchtools::batchtools_slurm`),
peer-to-peer (`future.p2p::cluster`). The user picks their backend via
`plan()` and zaro uses whatever's configured.

**User-level composability.** If someone writes their own chunk-processing
loop using `lapply()` over zaro results, piping `|> futurize()` just works.
This is valuable beyond zaro's own internals.

**Familiar code.** If you remove `|> futurize()`, the code runs sequentially.
This aligns with zaro's `parallel = FALSE` default — sequential by default,
parallel when asked.

### Sketch: futurize in zaro_read() internals

```r
# using futurize inside zaro_read()
results <- lapply(chunk_indices, function(cidx) {
  raw <- fetch_chunk(store, path, cidx, meta)
  if (is.null(raw)) return(NULL)
  list(idx = cidx, values = decode_chunk(raw, meta))
}) |> futurize()
```

Or using future.apply directly:

```r
results <- future.apply::future_lapply(chunk_indices, function(cidx) {
  raw <- fetch_chunk(store, path, cidx, meta)
  if (is.null(raw)) return(NULL)
  list(idx = cidx, values = decode_chunk(raw, meta))
})
```

## These are complementary, not competing

`future.mirai::mirai_multisession` is a future backend. futurize dispatches
to mirai under the hood when the user sets:

```r
plan(future.mirai::mirai_multisession, workers = 8)
```

So the full stack looks like:

```
User code
  └─ zaro_read(store, path, parallel = TRUE)
       └─ future_lapply() over chunk indices
            └─ dispatched via plan()
                 └─ mirai daemons (or multisession, or Slurm, etc.)
```

The user gets mirai's C-level dispatch performance and daemon connection
reuse, through futurize/future's backend-independent interface, with zero
changes to zaro's code.

## Design decision for zaro

**Use future as the parallelization interface, recommend mirai as the
backend.**

Concretely:

1. Add a `parallel` argument to `zaro_read()`, defaulting to `FALSE`.

2. When `parallel = TRUE`, use `future.apply::future_lapply()` for the
   chunk fetch+decode loop. This respects whatever `plan()` the user has
   set.

3. Add `future.apply` to Suggests (not Imports). When `parallel = TRUE`
   and future.apply is not available, fall back to sequential with a
   message.

4. Document mirai as the recommended backend:
   ```r
   library(future)
   plan(future.mirai::mirai_multisession, workers = 4)
   data <- zaro_read(store, "temperature", parallel = TRUE)
   ```

5. Don't manage daemons/workers inside zaro. The user controls their
   parallel backend lifecycle externally. This is the Futureverse
   convention and avoids surprising side effects.

## Store serialization

The main implementation detail is that store objects need to cross process
boundaries when sent to workers. The `ArrowStore` class wraps an Arrow R6
`FileSystem` object, which holds C++ pointers that don't serialize.

**Solution:** a `store_spec` pattern. The store knows how to describe itself
as a serializable specification (URI + auth parameters), and workers
reconstruct a fresh store on arrival. This is essentially what `zaro()`
already accepts as input — a URI string. So the worker-side code becomes:

```r
# on the worker
worker_store <- zaro(store_uri, ...)
raw <- fetch_chunk(worker_store, path, cidx, meta)
```

This means `zaro_read()` with `parallel = TRUE` would pass the store's
source URI and construction arguments to workers rather than the store
object itself. The `ZaroStore` class should expose a `store_uri()` method
or similar that returns the information needed to reconstruct it.

For `ReferenceStore` (Kerchunk), the refs list itself is pure R data and
serializes fine. The byte-range read operations on workers would reconstruct
Arrow/VSI filesystem handles as needed.

## Future considerations

### Chunk-level vs. slab-level parallelism

The current design parallelizes at the chunk level within a single
`zaro_read()` call. A higher-level parallelism — multiple `zaro_read()`
calls in parallel (e.g. reading different time steps) — is automatically
available through the future ecosystem without any zaro-specific code.
The user just wraps their loop in `future_lapply()` or `|> futurize()`.

### Adaptive parallelism

For small reads (1-2 chunks), parallel dispatch overhead isn't worth it.
`zaro_read()` could auto-detect this: if the number of chunks to fetch is
below a threshold (say 4), skip parallel dispatch regardless of the
`parallel` argument. This avoids the "I asked for parallel and it got
slower" experience.

### Connection pooling

For cloud stores, the ideal pattern is a pool of persistent connections
shared across chunk fetches. mirai's daemon model enables this naturally:
each daemon creates its store connection once (via `everywhere()`) and
reuses it for all chunks routed to that daemon. This is a significant
optimisation for HTTPS/S3 stores and aligns with mirai's
`everywhere()` + `.min` synchronization pattern:

```r
daemons(8)
everywhere({
  library(zaro)
  worker_store <- zaro("s3://bucket/dataset.zarr", anonymous = TRUE)
}, .min = 8)
```

### Progress reporting

mirai and the future ecosystem both support progress reporting. For large
reads spanning hundreds of chunks, a progress bar showing chunks fetched
vs. total would be valuable. This integrates naturally with the
`progressr` package which works with both future and mirai backends.
