# read-arrays.R -- several whole arrays in one fetch
#
# zaro_read_arrays()  plan every array, fetch all the plans together (one
#                     curl pool for HTTP), decode and assemble each array
#
# Opening a dataset reads its coordinate arrays: small, but one round trip
# each when read one at a time. On a high-latency host the round trips are
# the cost, so they are fetched together.


#' Read several whole arrays in one fetch
#'
#' Plans every array, fetches all the plans with one [zaro_fetch()] (so
#' HTTP chunks of all the arrays share one connection pool and go out
#' concurrently, and touching byte ranges merge), then decodes each array.
#' Each result is what `zaro_read(store, path)` returns. Useful for reading
#' a dataset's coordinate arrays at open, where each array is small and the
#' cost is the round trips.
#'
#' @param store a store from [zaro()], or a [zaro_dataset()] (then `paths`
#'   are array names in the dataset).
#' @param paths character vector of array paths (or dataset names).
#' @param metas optional list of array metadata, one per path (taken from
#'   the dataset when `store` is one, else read).
#' @inheritParams zaro_read
#' @returns A named list of arrays, one per path, each with `dim` in Zarr
#'   order as from [zaro_read()]. Fetch statistics are in
#'   `attr(, "stats")`.
#' @export
#' @examples
#' \dontrun{
#' ds <- zaro_dataset("https://host/store.zarr")
#' xy <- zaro_read_arrays(ds, c("x", "y"))
#' attr(xy, "stats")$requests
#' }
zaro_read_arrays <- function(store, paths, metas = NULL,
                             verbose = zaro_verbose()) {
  paths <- as.character(paths)
  if (!length(paths)) return(stats::setNames(list(), character()))
  if (is.null(metas)) metas <- vector("list", length(paths))
  if (length(metas) != length(paths)) {
    stop("'metas' must have one entry per path", call. = FALSE)
  }
  nms <- paths
  if (inherits(store, "zaro_dataset")) {
    res <- Map(function(p, m) dataset_array(store, p, m), paths, metas)
    paths <- vapply(res, `[[`, "", "path")
    metas <- lapply(res, `[[`, "meta")
    store <- store$store
  }
  metas <- Map(function(p, m) resolve_array_meta(store, p, m, verbose = FALSE),
               paths, metas)

  # plan each array (whole extent)
  jobs <- Map(function(p, m) {
    sel <- resolve_selection(m, NULL, NULL)
    chunks <- chunk_index_iter(chunk_coverage(sel$start, sel$count, m@chunk_shape))
    plan <- zaro_plan(store, p, meta = m, chunks = do.call(rbind, chunks),
                      verbose = FALSE)
    list(meta = m, sel = sel, chunks = chunks, plan = plan)
  }, paths, metas)

  # one fetch for every plan: the columns zaro_fetch() uses are common to
  # all plans (the dim_k columns differ with rank)
  cols <- Reduce(intersect, lapply(jobs, function(j) names(j$plan)))
  all_plans <- do.call(rbind, lapply(jobs, function(j) j$plan[cols]))
  which_job <- rep(seq_along(jobs), vapply(jobs, function(j) nrow(j$plan), 1L))
  raws <- zaro_fetch(store, all_plans, verbose = FALSE)
  stats <- attr(raws, "stats")
  vmsg("read ", length(jobs), " array(s), ", nrow(all_plans), " chunk(s) in ",
       stats$requests, " HTTP request(s)", verbose = verbose)

  out <- lapply(seq_along(jobs), function(k) {
    j <- jobs[[k]]
    rk <- raws[which_job == k]
    decoded <- lapply(seq_along(rk), function(i) {
      if (is.null(rk[[i]])) return(NULL)
      list(cidx = j$chunks[[i]],
           values = decode_planned(rk[[i]], j$meta, j$plan[i, ]))
    })
    a <- assemble_chunks(decoded, j$meta, j$sel$start, j$sel$count)
    if (!is.null(j$meta@dimension_names)) {
      names(dim(a)) <- j$meta@dimension_names
    }
    a
  })
  names(out) <- nms
  attr(out, "stats") <- stats
  out
}
