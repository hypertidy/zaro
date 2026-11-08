# mdimrefs.R -- read an mdim-refs byte-reference table as a Zarr V2 store
#
# A byterefs store (format "byterefs/0.1"; the legacy name "mdim-refs/0.2" is
# the same layout and is read during the transition, byterefs/spec decision
# 0003) is a SQLite database of chunk
# byte references: per-source chunk tables, an `arrays` table of array
# metadata, and one view per referenced array, `refs_<array>`, holding the
# global chunk index in the block-info row shape:
#
#   dim_0 .. dim_{n-1}, present, path, offset, size, info, data
#
# This backend answers Zarr V2 keys from that table: consolidated
# `.zmetadata` is synthesised from `arrays` once, at open, and chunk keys are
# looked up placement first (spec "One chunk by global key"), many at a time,
# then read as byte ranges of (path, offset, size).
# Coordinates held inline or as start/step are served as one whole-array chunk.
# `remap` rows of the chosen target rewrite the verbatim source paths; with
# no target given, `meta.default_target` is applied (decision 0004).

MdimRefsStore <- new_class("MdimRefsStore", parent = ZaroStore, properties = list(
  con    = class_any,        # DBI connection to the SQLite store
  remap  = class_data.frame, # prefix, replacement for the chosen target
  arrays = class_data.frame, # the arrays table
  target = class_character,  # the remap target applied ("" = verbatim)
  md     = class_list,       # synthesised Zarr V2 metadata, key -> document
  refused = class_character, # array -> why it cannot be read (decision 0007)
  cache  = class_environment # placements per array, in chunk units, read once
))

#' Open a byterefs (or legacy mdim-refs) store
#'
#' @param source path to the SQLite file, or an http(s) URL (downloaded once
#'   to a temporary file).
#' @param target remap target to apply to source paths (e.g. "public"). NULL
#'   (default) applies the store's `meta.default_target`, if any; `""` means
#'   the verbatim sources.
#' @noRd
open_mdimrefs <- function(source, target = NULL, verbose = zaro_verbose()) {
  for (pkg in c("DBI", "RSQLite")) if (!requireNamespace(pkg, quietly = TRUE))
    stop(pkg, " is required to read mdim-refs stores", call. = FALSE)
  if (grepl("^https?://", source)) {
    tf <- tempfile(fileext = ".sqlite")
    utils::download.file(source, tf, mode = "wb", quiet = !verbose)
    source <- tf
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), source, flags = RSQLite::SQLITE_RO)
  meta <- DBI::dbGetQuery(con, "SELECT key, value FROM meta")
  fmt <- meta$value[meta$key == "format_version"]
  if (!mdimrefs_format_ok(fmt)) {
    if (length(fmt) == 1L && !is.na(fmt) && startsWith(fmt, "byterefs/")) {
      warning("zaro reads byterefs/0.1; this store is ", fmt,
              ", read as byterefs/0.1", call. = FALSE)
    } else {
      stop("not a byterefs store (format_version ",
           if (length(fmt)) fmt else "missing", "; zaro reads ",
           paste(MR_FORMATS, collapse = " and "), "): ", source, call. = FALSE)
    }
  }
  vmsg("opening ", fmt, " store: ", source, verbose = verbose)

  # decision 0004: a reader not told a target applies meta.default_target
  if (is.null(target)) {
    dt <- meta$value[meta$key == "default_target"]
    target <- if (length(dt) == 1L && !is.na(dt)) dt else ""
    if (nzchar(target)) {
      vmsg("applying the store's default target '", target, "'", verbose = verbose)
    }
  }
  remap <- data.frame(prefix = character(), replacement = character())
  if (nzchar(target)) {
    remap <- tryCatch(
      DBI::dbGetQuery(con, "SELECT prefix, replacement FROM remap WHERE target = ?
                            ORDER BY length(prefix) DESC", params = list(target)),
      error = function(e) remap)
    if (!nrow(remap)) {
      warning("no remap rules for target '", target, "'; source paths are used verbatim",
              call. = FALSE)
    }
  }
  arrays <- DBI::dbGetQuery(con, "SELECT * FROM arrays")
  codecs <- mdimrefs_codecs(arrays)
  refused <- unlist(lapply(codecs, function(x) if (is.character(x)) x))
  if (length(refused)) {
    warning("cannot read ", length(refused), " array(s) of this store: ",
            paste0(names(refused), ": ", refused, collapse = "; "), call. = FALSE)
  }
  MdimRefsStore(root = source, con = con, remap = remap, arrays = arrays,
                target = target, md = mdimrefs_meta(arrays, codecs),
                refused = if (length(refused)) refused else
                  stats::setNames(character(), character()),
                cache = new.env(parent = emptyenv()))
}

## byterefs/spec decision 0003: a byterefs/0.1 reader accepts byterefs/0.1
## and, until byterefs/0.2 is tagged, the legacy mdim-refs/0.2 (same layout)
MR_FORMATS <- c("byterefs/0.1", "mdim-refs/0.2")

mdimrefs_format_ok <- function(fmt) {
  length(fmt) == 1L && !is.na(fmt) && fmt %in% MR_FORMATS
}

mdimrefs_json <- function(x) {
  if (is.null(x) || is.na(x)) return(NULL)
  jsonlite::fromJSON(x, simplifyVector = FALSE)
}

mdimrefs_meta <- function(A, codecs) {
  md <- list(".zgroup" = list(zarr_format = 2L), ".zattrs" = structure(list(), names = character()))
  for (i in seq_len(nrow(A))) {
    shape <- mdimrefs_json(A$shape[i])
    ref <- A$storage[i] == "referenced"
    codec <- if (ref && is.list(codecs[[A$array[i]]])) codecs[[A$array[i]]] else list()
    fill <- suppressWarnings(as.numeric(A$fill[i]))
    md[[paste0(A$array[i], "/.zarray")]] <- list(
      zarr_format = 2L, shape = shape,
      chunks = if (ref) mdimrefs_json(A$chunks[i]) else shape,
      dtype = A$dtype[i],
      compressor = if (ref) codec$compressor,
      filters = if (ref) codec$filters,
      fill_value = if (is.na(fill)) NULL else fill,
      order = "C", dimension_separator = ".")
    md[[paste0(A$array[i], "/.zattrs")]] <- c(
      list(`_ARRAY_DIMENSIONS` = mdimrefs_json(A$dims[i])),
      mdimrefs_json(A$attrs[i]))
  }
  md
}

mdimrefs_path <- function(store, path) {
  for (i in seq_len(nrow(store@remap))) {
    p <- store@remap$prefix[i]
    if (startsWith(path, p))
      return(paste0(store@remap$replacement[i], substring(path, nchar(p) + 1L)))
  }
  path
}

mdimrefs_coord_raw <- function(a) {
  if (a$storage == "inline") return(a$values[[1]])
  af <- mdimrefs_json(a$affine); n <- unlist(mdimrefs_json(a$shape))
  v <- af$start + af$step * (seq_len(n) - 1L)
  sz <- as.integer(sub("^.{2}", "", a$dtype))
  writeBin(if (substr(a$dtype, 2, 2) == "f") as.double(v) else as.integer(v), raw(),
           size = sz, endian = if (startsWith(a$dtype, ">")) "big" else "little")
}

method(store_get, MdimRefsStore) <- function(store, key) {
  md <- store@md
  if (key == ".zmetadata")
    return(charToRaw(to_json_exact(list(metadata = md, zarr_consolidated_format = 1L))))
  if (key %in% names(md))
    return(charToRaw(to_json_exact(md[[key]])))

  fetch_ref(store, store_refs(store, key))
}

# -- codecs (byterefs/spec decision 0007) -------------------------------------
#
# arrays.codec is a Zarr V3 codec chain (a JSON array of {name, configuration}
# in encode order) or, during byterefs/0.x, the V2 form {compressor, filters}
# (a JSON object). Both become the V2 compressor and filters of the
# synthesised .zarray; zaro decodes those in reverse, which is the chain's
# decode order. A name zaro does not know refuses the array (it is listed in
# the store's `refused` and reading it is an error), rather than misreading.

## V3 chain names zaro decodes, with the V2 id and the kind of codec
## (1 array-to-array, 2 array-to-bytes, 3 bytes-to-bytes)
MR_V3_CODECS <- data.frame(
  name = c("HorizontalDeltaCodec", "imagecodecs_delta", "bytes",
           "numcodecs.shuffle", "numcodecs.zlib", "zstd", "numcodecs.zstd",
           "gzip", "numcodecs.gzip", "blosc", "numcodecs.blosc",
           "crc32c", "numcodecs.crc32c"),
  id = c("imagecodecs_delta", "imagecodecs_delta", "bytes",
         "shuffle", "zlib", "zstd", "zstd", "gzip", "gzip", "blosc", "blosc",
         "crc32c", "crc32c"),
  kind = c(1L, 1L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L),
  stringsAsFactors = FALSE)

## V2 ids zaro decodes (decode_one() in codec.R)
MR_V2_IDS <- c("zlib", "gzip", "zstd", "lz4", "bz2", "brotli", "snappy",
               "blosc", "shuffle", "imagecodecs_delta", "horizontal_delta",
               "crc32c")

## each referenced array's codec as list(compressor, filters), or a character
## string saying why it cannot be read
mdimrefs_codecs <- function(A) {
  out <- list()
  for (i in seq_len(nrow(A))) {
    if (A$storage[i] != "referenced") next
    out[[A$array[i]]] <- tryCatch(mdimrefs_codec(A$codec[i], A$dtype[i]),
                                  error = function(e) conditionMessage(e))
  }
  out
}

mdimrefs_codec <- function(txt, dtype) {
  if (is.null(txt) || is.na(txt) || !nzchar(trimws(txt)))
    return(list(compressor = NULL, filters = NULL))
  x <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  ## tell the forms apart by JSON type: an array is a V3 chain
  if (startsWith(trimws(txt), "[")) return(mdimrefs_v3_chain(x, dtype))
  ids <- c(vapply(x$filters %||% list(), function(f) f$id %||% "", ""),
           x$compressor$id)
  bad <- setdiff(ids, MR_V2_IDS)
  if (length(bad)) stop("unknown codec ", paste0("'", bad, "'", collapse = ", "),
                        " in arrays.codec", call. = FALSE)
  list(compressor = x$compressor, filters = x$filters)
}

mdimrefs_v3_chain <- function(chain, dtype) {
  if (!length(chain)) stop("arrays.codec is an empty codec chain", call. = FALSE)
  nm <- vapply(chain, function(c) as.character(c$name %||% ""), "")
  if ("sharding_indexed" %in% nm)
    stop("sharding_indexed is not allowed in arrays.codec (a row is one chunk, ",
         "not a shard; byterefs decisions 0007, 0009)", call. = FALSE)
  bad <- setdiff(nm, MR_V3_CODECS$name)
  if (length(bad)) stop("unknown codec ", paste0("'", bad, "'", collapse = ", "),
                        " in arrays.codec", call. = FALSE)
  m <- MR_V3_CODECS[match(nm, MR_V3_CODECS$name), ]
  if (sum(m$kind == 2L) != 1L || is.unsorted(m$kind))
    stop("arrays.codec must be array-to-array codecs, then one array-to-bytes ",
         "codec ('bytes'), then bytes-to-bytes codecs; got ",
         paste(nm, collapse = ", "), call. = FALSE)
  cfg <- lapply(chain, function(c) c$configuration %||% list())
  ## the bytes codec's endian agrees with dtype (may be left out for 1 byte)
  e <- cfg[[which(m$kind == 2L)]]$endian
  bo <- substr(dtype, 1L, 1L)
  want <- switch(bo, "<" = "little", ">" = "big", NULL)
  if (is.null(e) && !is.null(want) && !endsWith(dtype, "1"))
    stop("the bytes codec has no endian for dtype '", dtype, "'", call. = FALSE)
  if (!is.null(e) && !is.null(want) && !identical(e, want))
    stop("the bytes codec's endian '", e, "' disagrees with dtype '", dtype, "'",
         call. = FALSE)
  itemsize <- as.integer(sub("^.[a-zA-Z]", "", dtype))
  v2 <- lapply(which(m$kind != 2L), function(k) {
    c <- c(list(id = m$id[k]), cfg[[k]])
    if (m$id[k] == "shuffle" && is.null(c$elementsize)) c$elementsize <- itemsize
    c
  })
  ## encode order: filters, then the compressor (zaro decodes in reverse)
  n <- length(v2)
  last_b2b <- n > 0L && m$kind[m$kind != 2L][n] == 3L
  list(compressor = if (last_b2b) v2[[n]],
       filters = if (n > as.integer(last_b2b)) v2[seq_len(n - last_b2b)])
}


# -- chunk lookup --------------------------------------------------------------
#
# A chunk key is looked up placement first, many keys at once (port of
# raad.virtualize mr_chunks(), byterefs spec "One chunk by global key"):
# the placements of the array are read once and matched to the keys, then the
# chunk rows are fetched by the chunk table's primary key. Filtering the
# refs_<array> view instead computes every row of the array for each key.
# Present with a path is a byte range (remapped), present without one is
# inline data, missing or not present is absent. Coordinates held inline or
# as start/step are one whole-array chunk.
method(store_refs, MdimRefsStore) <- function(store, keys) {
  out <- refs_frame(keys, transport = "absent")
  arr <- sub("/[^/]*$", "", keys); ck <- sub("^.*/", "", keys)
  ok <- grepl("^[0-9]+(\\.[0-9]+)*$", ck) & arr != keys
  for (a_name in unique(arr[ok])) {
    a <- store@arrays[store@arrays$array == a_name, , drop = FALSE]
    if (!nrow(a)) next
    rank <- length(mdimrefs_json(a$shape))
    parts <- strsplit(ck, ".", fixed = TRUE)
    w <- which(ok & arr == a_name & lengths(parts) == rank)    # keys of this array
    if (!length(w)) next
    idx <- matrix(as.integer(unlist(parts[w])), ncol = rank, byrow = TRUE)
    if (a$storage != "referenced") {
      at0 <- w[rowSums(idx != 0L) == 0L]
      for (i in at0) {
        out$data[[i]] <- mdimrefs_coord_raw(a)
        out$transport[i] <- "inline"
      }
      next
    }
    if (a_name %in% names(store@refused))
      stop("cannot read array '", a_name, "': ", store@refused[[a_name]], call. = FALSE)
    pl <- store@cache[[a_name]]
    if (is.null(pl)) pl <- store@cache[[a_name]] <-
      mdimrefs_placements(store@con, a_name, mdimrefs_json(a$chunks))
    r <- mdimrefs_chunks(store@con, a_name, idx, pl)
    r <- r[!duplicated(r$k) & r$present != 0L, , drop = FALSE]   # absent: fill value
    if (!nrow(r)) next
    i <- w[r$k]
    inl <- is.na(r$offset)
    for (j in which(inl)) {                                   # inline chunk
      out$data[[i[j]]] <- as.raw(r$data[[j]])
      out$transport[i[j]] <- "inline"
    }
    ref <- !inl
    ## decision 0008: size NULL with offset 0 is the whole object; with any
    ## other offset the row is invalid
    bad <- ref & is.na(r$size) & r$offset != 0
    if (any(bad))
      stop("invalid chunk row for '", keys[i[which(bad)[1L]]], "': size is NULL with ",
           "offset ", r$offset[which(bad)[1L]], " (only offset 0, the whole object, ",
           "may have no size; byterefs decision 0008)", call. = FALSE)
    if (any(ref)) {
      p <- vapply(r$path[ref], function(x) mdimrefs_path(store, x), "", USE.NAMES = FALSE)
      out$path[i[ref]] <- p
      out$offset[i[ref]] <- as.numeric(r$offset[ref])
      out$size[i[ref]] <- as.numeric(r$size[ref])
      out$transport[i[ref]] <- url_transport(p)
    }
  }
  out
}

## the placements of array `a` in chunk units of chunk shape `ch`: lo and hi
## (the slab's first and one-past-last chunk on each axis), sk (source key =
## global key + sk), source_id, and their order on the search axis; or
## list(view = TRUE) when the store has no placements table
mdimrefs_placements <- function(con, a, ch) {
  if (!DBI::dbExistsTable(con, "placements")) return(list(view = TRUE))
  ch <- as.integer(unlist(ch)); rank <- length(ch)
  P <- DBI::dbGetQuery(con, "SELECT source_id, src_offset, count, dest FROM placements
                             WHERE array = ?", params = list(a))
  ## each value is a flat JSON array of integers; split rather than parse
  m <- function(x) matrix(as.numeric(unlist(strsplit(gsub("[][[:space:]]", "", x), ",",
                                                     fixed = TRUE))),
                          ncol = rank, byrow = TRUE)
  chm <- matrix(ch, nrow(P), rank, byrow = TRUE)
  de <- m(P$dest); lo <- de %/% chm
  hi <- ceiling((de + m(P$count)) / chm)
  sk <- m(P$src_offset) %/% chm - lo
  ## chunk units fit integers; integers also print plainly in the SQL
  storage.mode(lo) <- storage.mode(hi) <- storage.mode(sk) <- "integer"
  ## search on the axis with the most distinct slab starts
  ax <- if (nrow(P)) which.max(apply(lo, 2L, function(v) length(unique(v)))) else 1L
  o <- order(lo[, ax])
  list(view = FALSE, lo = lo, hi = hi, sk = sk, source_id = as.integer(P$source_id),
       ax = ax, o = o, los = lo[o, ax], span = if (nrow(P)) max(hi[, ax] - lo[, ax]) else 0L)
}

## chunk rows for the global keys `idx` (one row per key, one column per
## axis) of array `a`, with `pl` its placements from mdimrefs_placements().
## Returns k (the row of idx), present, path (the verbatim source), offset,
## size, data, for the keys that have a row; keys no placement holds are
## missing. The path is remapped by the caller (mdimrefs_path(), the longest
## matching prefix, as the view does).
mdimrefs_chunks <- function(con, a, idx, pl, batch = 10000L) {
  storage.mode(idx) <- "integer"
  rank <- ncol(idx)
  empty <- data.frame(k = integer(), present = integer(), path = character(),
                      offset = numeric(), size = numeric())
  empty$data <- list()
  if (isTRUE(pl$view)) return(mdimrefs_chunks_view(con, a, idx, empty, batch))
  if (!length(pl$source_id)) return(empty)
  lo <- pl$lo; hi <- pl$hi; sk <- pl$sk; sid <- pl$source_id
  ax <- pl$ax; o <- pl$o; los <- pl$los; span <- pl$span
  ## candidates: placements whose start on axis `ax` is in (q - span, q]
  up <- findInterval(idx[, ax], los)
  dn <- findInterval(idx[, ax] - span, los)
  n <- pmax(up - dn, 0L)
  kk <- rep(seq_len(nrow(idx)), n)
  pp <- o[unlist(lapply(which(n > 0L), function(r) (dn[r] + 1L):up[r]))]
  if (!length(kk)) return(empty)
  inside <- rowSums(idx[kk, , drop = FALSE] >= lo[pp, , drop = FALSE] &
                    idx[kk, , drop = FALSE] <  hi[pp, , drop = FALSE]) == rank
  kk <- kk[inside]; pp <- pp[inside]
  if (!length(kk)) return(empty)
  src <- idx[kk, , drop = FALSE] + sk[pp, , drop = FALSE]
  vals <- do.call(paste, c(as.data.frame(cbind(kk, sid[pp], src)), sep = ", "))
  i <- seq_len(rank) - 1L
  tbl <- DBI::dbQuoteIdentifier(con, a)
  q <- function(v) sprintf('WITH q(k, source_id, %s) AS (VALUES (%s))
SELECT q.k, c.present, CASE WHEN c."offset" IS NULL THEN NULL ELSE s.source END AS path,
       c."offset" AS "offset", c.size AS size, c.data AS data
  FROM q JOIN %s c ON c.source_id = q.source_id AND %s
         JOIN sources s ON s.source_id = c.source_id
  ORDER BY q.k',
    paste0("s_", i, collapse = ", "), paste(v, collapse = "), ("), tbl,
    paste(sprintf("c.dim_%d = q.s_%d", i, i), collapse = " AND "))
  chunks <- split(vals, ceiling(seq_along(vals) / batch))
  r <- do.call(rbind, lapply(chunks, function(v) mdimrefs_typed(DBI::dbGetQuery(con, q(v)))))
  r[order(r$k), , drop = FALSE]
}

## no placements table (not a conformant byterefs store): one query of the
## view per batch of keys, joined on the view's keys
mdimrefs_chunks_view <- function(con, a, idx, empty, batch = 10000L) {
  i <- seq_len(ncol(idx)) - 1L
  storage.mode(idx) <- "integer"
  vals <- do.call(paste, c(as.data.frame(cbind(seq_len(nrow(idx)), idx)), sep = ", "))
  q <- function(v) sprintf('WITH q(k, %s) AS (VALUES (%s))
SELECT q.k, r.present, r.path, r."offset" AS "offset", r.size AS size, r.data AS data
  FROM q JOIN %s r ON %s ORDER BY q.k',
    paste0("d_", i, collapse = ", "), paste(v, collapse = "), ("),
    DBI::dbQuoteIdentifier(con, paste0("refs_", a)),
    paste(sprintf("r.dim_%d = q.d_%d", i, i), collapse = " AND "))
  chunks <- split(vals, ceiling(seq_along(vals) / batch))
  r <- do.call(rbind, lapply(chunks, function(v) mdimrefs_typed(DBI::dbGetQuery(con, q(v)))))
  if (is.null(r)) empty else r
}

## RSQLite types an all-NULL column as logical
mdimrefs_typed <- function(r) {
  r$path <- as.character(r$path)
  r$offset <- as.numeric(r$offset)
  r$size <- as.numeric(r$size)
  if (!is.list(r$data)) r$data <- vector("list", nrow(r))
  r
}

method(store_list, MdimRefsStore) <- function(store, prefix = "") {
  keys <- c(".zmetadata", names(store@md))
  if (nzchar(prefix)) keys[startsWith(keys, prefix)] else keys
}

method(store_exists, MdimRefsStore) <- function(store, key) {
  !is.null(store_get(store, key))
}
