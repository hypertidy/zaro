# plan.R -- zaro_plan(): the refs table for a selection, before any read
#
# address --resolve--> store --plan(selection)--> refs table --fetch--> raw
#   --decode--> array
#
# zaro_plan() is the "plan" step made visible: which chunks a selection
# needs, where each one's bytes are, and what reading them will cost.


#' Plan a read: the byte references a selection needs, without reading them
#'
#' Works out which chunks of an array a hyperslab touches and where each
#' chunk's bytes are: a whole object in a native store, a byte range inside
#' a NetCDF/HDF5/TIFF file for a virtual store (kerchunk, VirtualiZarr,
#' byterefs), a byte range inside a shard object for a sharded array, inline
#' bytes, or absent (reads as `fill_value`). No chunk data
#' is read. For a native unsharded store no request is made at all; a
#' sharded array reads the index of each shard it touches, and a virtual
#' store may fetch the manifest or row groups it needs (cached in the
#' store).
#'
#' The result is a data frame you can inspect, filter, save or hand to
#' another tool. Printing it summarises the cost: requests, known bytes and
#' hosts.
#'
#' @param store a store from [zaro()], or a [zaro_dataset()].
#' @param path character. Path to the array within the store.
#' @param start,count the selection, as for [zaro_read()]: 0-based start and
#'   element counts in Zarr dimension order, `NA` count for the rest of an
#'   axis. Default the whole array.
#' @param meta optional array metadata, as for [zaro_read()].
#' @param chunks optional integer matrix of 0-based chunk grid indices (one
#'   row per chunk, Zarr dimension order), instead of `start`/`count`.
#' @param verbose logical. Emit diagnostic messages. Default
#'   `getOption("zaro.verbose", TRUE)`.
#' @returns A `zaro_plan` data frame, one row per chunk, in the shape of a
#'   byterefs view (\url{https://github.com/byterefs/spec}):
#'   `dim_0`, ..., `dim_{n-1}` (the chunk's 0-based grid index, Zarr
#'   dimension order), `present` (`FALSE` for an absent chunk, read as
#'   `fill_value`), `path` (where the bytes are), `offset`, `size` (`NA` for
#'   a whole object of unknown size), `data` (list of raw, for inline
#'   chunks), then zaro's own columns: `key` (the Zarr key) and `transport`
#'   (`"store"`, `"http"`, `"s3"`, `"gs"`, `"vsi"`, `"local"`, `"inline"` or
#'   `"absent"`: how the bytes are fetched). Attributes `array` (the array's
#'   path in the store) and `meta` record what was planned.
#'
#' @export
#' @examples
#' \dontrun{
#' store <- zaro(paste0("virtualizarr://https://raw.githubusercontent.com/",
#'   "mdsumner/virtualized/refs/heads/main/remote/ocean_temp_2023.parq"))
#' p <- zaro_plan(store, "temp", start = c(0, 0, 0, 0), count = c(1, 1, NA, NA))
#' p                          # 60 byte ranges on one host, ~4 MB
#' table(p$transport)
#' }
zaro_plan <- function(store, path = "", start = NULL, count = NULL,
                      meta = NULL, chunks = NULL, verbose = zaro_verbose()) {
  if (path == ".") path <- ""
  if (inherits(store, "zaro_dataset")) {
    a <- dataset_array(store, path, meta)
    store <- a$store; path <- a$path; meta <- a$meta
  }
  meta <- resolve_array_meta(store, path, meta, verbose)

  if (is.null(chunks)) {
    sel <- resolve_selection(meta, start, count)
    ranges <- chunk_coverage(sel$start, sel$count, meta@chunk_shape)
    chunks <- as.matrix(expand.grid(ranges, KEEP.OUT.ATTRS = FALSE))
  }
  chunks <- matrix(as.integer(chunks), ncol = length(meta@shape))
  grid <- ceiling(meta@shape / meta@chunk_shape)
  if (any(chunks < 0L) || any(sweep(chunks, 2L, grid, `>=`))) {
    stop("chunk index outside the chunk grid (", paste(grid, collapse = " x "),
         ")", call. = FALSE)
  }

  shard <- attr(meta, "shard")
  if (is.null(shard)) {
    ck <- apply(chunks, 1L, function(i) chunk_key(meta, i))
    keys <- if (nzchar(path)) paste0(path, "/", ck) else ck
    plan <- store_refs(store, keys)
  } else {
    plan <- plan_sharded(store, path, meta, chunks, shard)
  }
  dims <- as.data.frame(chunks)
  names(dims) <- paste0("dim_", seq_len(ncol(chunks)) - 1L)
  plan$present <- plan$transport != "absent"
  plan <- cbind(dims, plan[c("present", "path", "offset", "size")],
                plan["data"], plan[c("key", "transport")])
  vmsg("planned ", nrow(plan), " chunk(s) for path '", path, "'",
       verbose = verbose)
  structure(plan, class = c("zaro_plan", "data.frame"), array = path,
            meta = meta)
}


# -- sharding -----------------------------------------------------------------
#
# A shard is one more level of byte references. Its object holds an index of
# (offset, nbytes) per inner chunk in C order (uint64 pairs, all 0xFF for an
# empty chunk), at the end or start of the object, encoded by index_codecs
# (bytes, optionally crc32c: stripped, not verified). Codecs after
# sharding_indexed apply to the whole shard object: a crc32c there is 4
# trailing bytes (skipped); a compressor means whole shards are fetched and
# decoded at plan time (their inner chunks become inline rows). Planning reads the
# indexes of the shards a selection touches -- one small ranged read each,
# fetched together -- and turns every inner chunk into a byte-range row of
# its shard object. A missing shard leaves all its inner chunks absent.

#' Plan inner chunks of a sharded array
#' @noRd
plan_sharded <- function(store, path, meta, chunks, shard) {
  per <- shard$shape %/% meta@chunk_shape            # inner chunks per shard
  n_inner <- prod(per)
  sidx <- sweep(chunks, 2L, per, `%/%`)
  iidx <- sweep(chunks, 2L, per, `%%`)
  stride <- rev(cumprod(rev(c(per[-1L], 1))))
  lin <- as.numeric(iidx %*% stride) + 1             # 1-based, C order
  sk <- apply(sidx, 1L, function(i) chunk_key(meta, i))
  skeys <- if (nzchar(path)) paste0(path, "/", sk) else sk

  ushards <- unique(skeys)
  srefs <- store_refs(store, ushards)
  if (!shard_ranged(shard)) {
    # a compressor on the whole shard: no ranged reads, so fetch whole shards,
    # decode them, and plan their inner chunks as inline bytes
    whole <- srefs
    whole$size[whole$transport != "inline"] <- NA_real_
    whole$offset[whole$transport != "inline"] <- 0
    raws <- zaro_fetch(store, whole, verbose = FALSE)
    for (i in seq_along(raws)) {
      if (is.null(raws[[i]])) {
        srefs$transport[i] <- "absent"
        next
      }
      b <- raws[[i]]
      for (codec in rev(shard$outer_codecs)) b <- decode_one(b, codec, meta)
      srefs$data[[i]] <- b
      srefs$transport[i] <- "inline"
      srefs$path[i] <- NA_character_
    }
    shard$outer_codecs <- list()
  }
  tabs <- shard_indexes(store, srefs, n_inner, shard)

  plan <- refs_frame(skeys, transport = "absent")
  for (i in seq_along(skeys)) {
    m <- match(skeys[i], ushards)
    tab <- tabs[[m]]
    if (is.null(tab) || tab$empty[lin[i]]) next
    o <- tab$offset[lin[i]]
    n <- tab$nbytes[lin[i]]
    ref <- srefs[m, , drop = FALSE]
    base <- if (!is.na(ref$size)) ref$offset else 0
    if (ref$transport == "inline") {
      plan$data[[i]] <- ref$data[[1L]][seq.int(o + 1, length.out = n)]
      plan$transport[i] <- "inline"
      next
    }
    plan$path[i] <- ref$path
    plan$offset[i] <- base + o
    plan$size[i] <- n
    plan$transport[i] <- ref$transport
  }
  plan
}

#' Can a shard be read by byte ranges? Only if nothing but crc32c follows
#' sharding_indexed (a checksum appended to the shard object).
#' @noRd
shard_ranged <- function(shard) {
  all(vapply(shard$outer_codecs %||% list(),
             function(c) identical(c$name, "crc32c"), logical(1)))
}

#' Read and decode the index of each shard (NULL for a missing shard)
#' @noRd
shard_indexes <- function(store, srefs, n_inner, shard) {
  crc <- sum(vapply(shard$index_codecs, function(c) identical(c$name, "crc32c"),
                    logical(1)))
  size <- 16 * n_inner + 4 * crc
  at_end <- !identical(shard$index_location, "start")
  # crc32c on the whole shard: 4 bytes after the index (end) or the data
  tail <- 4 * length(shard$outer_codecs %||% list())
  if (at_end) size <- size + tail

  # where each index is: within a known byte range, or relative to the object
  rows <- srefs
  known <- !is.na(srefs$size)
  rows$offset <- ifelse(known, if (at_end) srefs$offset + srefs$size - size
                                  else srefs$offset,
                        if (at_end) -1 else 0)
  rows$size <- size
  raw <- vector("list", nrow(rows))
  http <- which(rows$transport == "http")
  if (length(http)) {
    raw[http] <- http_fetch(rows$path[http], rows$offset[http], rows$size[http],
                            max_con = getOption("zaro.max_con", 8L),
                            absent_ok = rep(TRUE, length(http)))
  }
  for (i in setdiff(seq_len(nrow(rows)), http)) {
    r <- rows[i, , drop = FALSE]
    if (r$transport == "inline") {
      b <- r$data[[1L]]
      raw[[i]] <- if (at_end) b[seq.int(length(b) - size + 1, length(b))] else b[seq_len(size)]
    } else {
      raw[i] <- list(fetch_ref(store, r))
    }
  }
  if (at_end && tail > 0) {
    raw <- lapply(raw, function(b) if (is.null(b)) NULL else b[seq_len(length(b) - tail)])
  }
  lapply(raw, decode_shard_index, n_inner = n_inner, shard = shard)
}

#' Shard index bytes -> data.frame(offset, nbytes, empty)
#' @noRd
decode_shard_index <- function(raw, n_inner, shard) {
  if (is.null(raw)) return(NULL)
  for (codec in rev(shard$index_codecs)) {
    if (identical(codec$name, "crc32c")) raw <- raw[seq_len(length(raw) - 4L)]
  }
  if (length(raw) != 16 * n_inner) {
    stop("shard index has ", length(raw), " bytes, expected ", 16 * n_inner,
         call. = FALSE)
  }
  endian <- chunk_endian(shard$index_codecs)
  v <- read_int64(raw, 2L * n_inner, endian, signed = FALSE)
  ff <- matrix(raw == as.raw(0xff), nrow = 8L)
  empty_word <- colSums(ff) == 8L
  data.frame(offset = v[c(TRUE, FALSE)], nbytes = v[c(FALSE, TRUE)],
             empty = empty_word[c(TRUE, FALSE)] & empty_word[c(FALSE, TRUE)])
}


#' @export
print.zaro_plan <- function(x, ...) {
  s <- plan_summary(x)
  cat(sprintf("<zaro_plan> '%s': %d chunk(s), %d request(s)\n",
              attr(x, "array"), s$chunks, s$requests))
  by <- paste(sprintf("%s %d", names(s$transport), s$transport), collapse = ", ")
  cat("  transport: ", by, "\n", sep = "")
  if (s$requests) {
    known <- s$requests - s$unknown
    parts <- c(if (known) format_bytes(s$bytes),
               if (s$unknown) sprintf("%d whole object(s) of unknown size",
                                      s$unknown))
    cat("  bytes:     ", paste(parts, collapse = " + "), "\n", sep = "")
    if (length(s$hosts)) {
      cat("  hosts:     ", paste(s$hosts, collapse = ", "), "\n", sep = "")
    }
  }
  show <- utils::head(as.data.frame(x)[c("key", "transport", "offset",
                                         "size", "path")], 6L)
  long <- !is.na(show$path) & nchar(show$path) > 36L
  show$path[long] <- paste0("...", substring(show$path[long],
                                            nchar(show$path[long]) - 32L))
  print(show, row.names = FALSE)
  if (nrow(x) > 6L) cat("  ... ", nrow(x) - 6L, " more\n", sep = "")
  invisible(x)
}


#' Requests, bytes and hosts of a plan
#' @noRd
plan_summary <- function(plan) {
  fetch <- !plan$transport %in% c("inline", "absent")
  len <- plan$size[fetch]
  url <- plan$path[fetch]
  host <- ifelse(grepl("^[a-z0-9+]+://", url),
                 sub("^[a-z0-9+]+://([^/]+).*$", "\\1", url), NA_character_)
  list(chunks = nrow(plan), requests = sum(fetch),
       bytes = sum(len, na.rm = TRUE), unknown = sum(is.na(len)),
       transport = table(factor(plan$transport,
                                levels = unique(plan$transport))),
       hosts = sort(unique(stats::na.omit(host))))
}


#' Resolve 0-based start/count (NA count = rest of axis), clamped to the array
#' @noRd
resolve_selection <- function(meta, start = NULL, count = NULL) {
  ndim <- length(meta@shape)
  if (is.null(start)) start <- rep(0L, ndim)
  if (is.null(count)) count <- rep(NA_integer_, ndim)
  start <- as.integer(start)
  count <- as.integer(count)
  if (length(start) != ndim || length(count) != ndim) {
    stop("start and count must have length ", ndim, call. = FALSE)
  }
  na_idx <- is.na(count)
  count[na_idx] <- meta@shape[na_idx] - start[na_idx]
  if (any(start + count > meta@shape)) {
    over <- which(start + count > meta@shape)
    warning("request exceeds array extent on dimension(s) ",
            paste(over, collapse = ", "), "; clamping", call. = FALSE)
    count <- pmin(count, meta@shape - start)
  }
  list(start = start, count = count)
}
