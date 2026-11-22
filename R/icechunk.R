# icechunk.R - Icechunk repositories as a zaro store, through tabularberg
#
# An Icechunk session is a key-value view of one snapshot, so it is a Zarr
# store: metadata and chunks are read by key with tabularberg::ic_get(). What
# makes it fit zaro is the manifest: tabularberg::ic_chunk_refs() says where
# each chunk's bytes are, which is exactly zaro's refs table (refs.R):
#
#   kind      location                 refs row
#   native    chunks/<id> (repo)       transport "store"  (ic_get_many)
#   inline    (bytes in the manifest)  transport "inline" (data column)
#   virtual   URL of another object    transport "store", or with
#                                      direct = TRUE its URL and byte range
#                                      (http/s3/gs/local) for zaro_fetch()
#   (no row)                           transport "absent" (fill value)
#
# Through icechunk (the default) every read gets Icechunk's container
# authorization and its etag/last-modified checks. direct = TRUE lets zaro
# fetch virtual chunks itself (one curl pool, merged byte ranges), but only
# from containers the repository has authorized, and without the checks.
#
# A read-only session is pinned to one snapshot, so a lazy zaro_array() made
# from it keeps reading the same bytes when the branch moves on.

IcechunkStore <- new_class("IcechunkStore", parent = ZaroStore, properties = list(
  session = class_any,                 # tabularberg icechunk_session
  snapshot = class_character,          # snapshot id the session is pinned to
  authorized = class_character,        # container prefixes zaro may read directly
  direct = new_property(class_logical, default = FALSE),
  cache = class_environment            # per-array chunk refs, by array path
))


#' Open an Icechunk repository as a zaro store
#'
#' Reads one snapshot of an [Icechunk](https://icechunk.io) repository
#' through the tabularberg package. Everything zaro does with a store works on
#' the result: [zaro_meta()], [zaro_read()], [zaro_array()],
#' [zaro_dataset()], [zaro_plan()].
#'
#' The tabularberg package is not on CRAN; install it from R-universe with
#' `install.packages("tabularberg", repos = "https://hypertidy.r-universe.dev")`.
#'
#' The session is read-only and pinned to one snapshot (its id is in
#' `store@snapshot`), so lazy arrays made from it keep reading the same
#' version after the branch moves on. Pick the version with `branch`, `tag`
#' or `snapshot` (default branch `"main"`).
#'
#' Chunks are read through Icechunk by default, which applies the
#' repository's container authorization and checksums. With `direct = TRUE`
#' zaro fetches virtual chunks itself, concurrently and with adjacent byte
#' ranges merged, but only from containers the repository object has
#' authorized (see `tabularberg::ic_repo_containers()`), and without
#' Icechunk's etag / last-modified checks. Native and inline chunks always
#' come from Icechunk.
#'
#' @param source an `icechunk_repo` or `icechunk_session` from tabularberg,
#'   or a repository location for `tabularberg::ic_repo_open()` (a local path,
#'   an `http(s)://` URL, or an `icechunk_storage`).
#' @param branch,tag,snapshot the version to read (one of them; default
#'   branch `"main"`). Ignored when `source` is a session.
#' @param authorize virtual chunk containers to authorize, passed to
#'   `tabularberg::ic_repo_open()` when `source` is a location.
#' @param direct logical. Fetch virtual chunks with zaro's own fetcher
#'   instead of through Icechunk (see Details).
#' @param verbose logical. Emit diagnostic messages.
#' @returns An `IcechunkStore`.
#' @export
#' @examplesIf requireNamespace("tabularberg", quietly = TRUE)
#' ## a small local repository that tabularberg writes to tempdir()
#' ex <- tabularberg::ic_example_repo()
#' store <- zaro_icechunk(ex$path, tag = "v1", authorize = ex$authorize)
#' zaro_dataset(store)
#' zaro_read(store, "from_bin", start = c(0, 0), count = c(3, 4))
#' if (requireNamespace("altarr", quietly = TRUE)) {
#'   x <- zaro_array(store, "from_bin")
#'   x[1:4, 1:4]
#' }
zaro_icechunk <- function(source, branch = NULL, tag = NULL, snapshot = NULL,
                          authorize = NULL, direct = FALSE,
                          verbose = zaro_verbose()) {
  if (!requireNamespace("tabularberg", quietly = TRUE)) {
    stop("Icechunk repositories need the tabularberg package (not on CRAN):\n",
         "  install.packages(\"tabularberg\", repos = \"https://hypertidy.r-universe.dev\")",
         call. = FALSE)
  }
  repo <- NULL
  if (inherits(source, "icechunk_session")) {
    session <- source
  } else {
    repo <- if (inherits(source, "icechunk_repo")) source else
      tabularberg::ic_repo_open(source, authorize = authorize)
    if (is.null(branch) && is.null(tag) && is.null(snapshot)) branch <- "main"
    session <- tabularberg::ic_session_readonly(repo, branch = branch, tag = tag,
                                              snapshot = snapshot)
  }
  info <- tabularberg::ic_session_info(session)
  if (!isTRUE(info$read_only)) {
    stop("zaro reads Icechunk through a read-only session, so a lazy array ",
         "stays on one snapshot; use tabularberg::ic_session_readonly()",
         call. = FALSE)
  }
  authorized <- character()
  if (isTRUE(direct)) {
    if (is.null(repo)) {
      stop("direct = TRUE needs the repository (to know which containers ",
           "are authorized); pass the icechunk_repo, not a session",
           call. = FALSE)
    }
    ct <- tabularberg::ic_repo_containers(repo)
    authorized <- ct$prefix[ct$authorized]
  }
  root <- if (!is.null(repo)) format_ic_storage(repo) else "icechunk session"
  vmsg("opening Icechunk snapshot ", info$snapshot_id, verbose = verbose)
  IcechunkStore(root = root, session = session,
                snapshot = as.character(info$snapshot_id),
                authorized = as.character(authorized),
                direct = isTRUE(direct), cache = new.env(parent = emptyenv()))
}


#' A one-line label for a repository's storage
#' @noRd
format_ic_storage <- function(repo) {
  out <- tryCatch(utils::capture.output(print(repo$storage)),
                  error = function(e) character())
  if (length(out)) trimws(sub("^<icechunk_storage>", "", out[1L])) else
    "icechunk repository"
}


#' Parse an icechunk:// source: icechunk://<location>?branch=|tag=|snapshot=
#' @noRd
parse_icechunk_url <- function(source) {
  x <- sub("^icechunk://", "", source)
  q <- list()
  if (grepl("?", x, fixed = TRUE)) {
    query <- sub("^[^?]*\\?", "", x)
    x <- sub("\\?.*$", "", x)
    for (kv in strsplit(query, "&", fixed = TRUE)[[1L]]) {
      p <- strsplit(kv, "=", fixed = TRUE)[[1L]]
      if (length(p) == 2L && p[1L] %in% c("branch", "tag", "snapshot")) {
        q[[p[1L]]] <- utils::URLdecode(p[2L])
      }
    }
  }
  c(list(location = x), q)
}


# -- key access ----------------------------------------------------------------

method(store_get, IcechunkStore) <- function(store, key) {
  tabularberg::ic_get(store@session, sub("^/+", "", key))
}

method(store_range, IcechunkStore) <- function(store, key, offset, length) {
  key <- sub("^/+", "", key)
  if (offset < 0) {
    raw <- tabularberg::ic_get(store@session, key)
    if (is.null(raw)) return(NULL)
    return(raw[seq.int(base::length(raw) - length + 1, length.out = length)])
  }
  tabularberg::ic_get(store@session, key, offset = offset, length = length)
}

method(store_exists, IcechunkStore) <- function(store, key) {
  isTRUE(tabularberg::ic_exists(store@session, sub("^/+", "", key)))
}

# Icechunk lists node prefixes only (a group or an array path, not "x/c")
method(store_list, IcechunkStore) <- function(store, prefix = "", recursive = FALSE) {
  prefix <- sub("^/+", "", sub("/+$", "", prefix))
  keys <- tabularberg::ic_list(store@session, prefix = prefix,
                             recursive = TRUE)
  keys <- sub("^/+", "", as.character(keys))
  if (!isTRUE(recursive)) {
    # one level below prefix, like a directory listing
    rel <- if (nzchar(prefix)) substring(keys, nchar(prefix) + 2L) else keys
    top <- unique(sub("/.*$", "", rel[nzchar(rel)]))
    keys <- if (nzchar(prefix)) paste0(prefix, "/", top) else top
  }
  keys
}


# -- chunk references ------------------------------------------------------------

#' The chunk refs of one array for some keys, from the manifest
#'
#' A small array's refs are read whole, once, and cached on the store. An
#' array with more chunks than `getOption("zaro.icechunk_whole_refs", 1e6)`
#' is asked only about the slabs of the chunk grid that hold the keys
#' (tabularberg::ic_chunk_refs(chunks =)), so its manifest is never in
#' memory whole; that needs a tabularberg with the `chunks` argument.
#' @noRd
ic_array_refs <- function(store, array, keys = NULL) {
  hit <- store@cache[[array]]
  if (!is.null(hit)) return(hit)
  if (!is.null(keys) && ic_has_slab() &&
      ic_array_nchunks(store, array) > getOption("zaro.icechunk_whole_refs", 1e6)) {
    return(ic_slab_refs(store, array, keys))
  }
  refs <- tabularberg::ic_chunk_refs(store@session, path = paste0("/", array),
                                   inline = TRUE)
  refs$key <- sub("^/+", "", refs$key)
  assign(array, refs, envir = store@cache)
  refs
}

#' Does the installed tabularberg read chunk refs by slab?
#' @noRd
ic_has_slab <- function() {
  "chunks" %in% names(formals(tabularberg::ic_chunk_refs))
}

#' Chunks in an array's grid (0 if it cannot be told), from the snapshot
#' @noRd
ic_array_nchunks <- function(store, array) {
  nodes <- store@cache[[".nodes"]]
  if (is.null(nodes)) {
    nodes <- tryCatch(tabularberg::ic_nodes(store@session), error = function(e) NULL)
    if (is.null(nodes)) return(0)
    assign(".nodes", nodes, envir = store@cache)
  }
  i <- match(paste0("/", array), nodes$path)
  if (is.na(i) || is.null(nodes$num_chunks) || is.na(nodes$num_chunks[i])) return(0)
  n <- suppressWarnings(as.numeric(strsplit(nodes$num_chunks[i], ",", fixed = TRUE)[[1L]]))
  if (!length(n) || anyNA(n)) 0 else prod(n)
}

#' Refs for some chunk keys of a large array, by slab
#'
#' The keys' chunk indices are grouped into boxes (ic_boxes()) and each box
#' is one ranged manifest read. The latest boxes are kept on the store, so
#' reading the same region again asks nothing.
#' @noRd
ic_slab_refs <- function(store, array, keys) {
  rel <- substring(keys, nchar(array) + if (nzchar(array)) 2L else 1L)   # "c/0/1"
  parts <- strsplit(sub("^c/?", "", rel), "/", fixed = TRUE)
  nd <- max(lengths(parts))
  if (nd == 0L || any(lengths(parts) != nd)) {            # scalar or odd keys
    refs <- tabularberg::ic_chunk_refs(store@session, path = paste0("/", array),
                                     inline = TRUE)
    refs$key <- sub("^/+", "", refs$key)
    return(refs)
  }
  idx <- matrix(as.numeric(unlist(parts)), ncol = nd, byrow = TRUE)
  idx <- idx[!duplicated(idx), , drop = FALSE]
  ck <- paste0(".slabs:", array)
  slabs <- store@cache[[ck]] %||% list()
  out <- list()
  for (b in ic_boxes(idx)) {
    have <- NULL
    for (s in slabs) {
      if (all(s$lo <= b$lo) && all(s$hi >= b$hi)) { have <- s; break }
    }
    if (is.null(have)) {
      chunks <- stats::setNames(lapply(seq_len(nd), function(d) c(b$lo[d], b$hi[d] + 1)),
                                paste0("dim_", seq_len(nd) - 1L))
      refs <- tabularberg::ic_chunk_refs(store@session, path = paste0("/", array),
                                       inline = TRUE, chunks = chunks)
      refs$key <- sub("^/+", "", refs$key)
      have <- list(lo = b$lo, hi = b$hi, refs = refs)
      slabs <- c(slabs, list(have))
    }
    out <- c(out, list(have$refs))
  }
  keep <- getOption("zaro.icechunk_cache_slabs", 16L)
  if (length(slabs) > keep) slabs <- utils::tail(slabs, keep)
  assign(ck, slabs, envir = store@cache)
  if (length(out) == 1L) out[[1L]] else do.call(rbind, out)
}

#' Boxes of the chunk grid that hold some chunk indices
#'
#' A slab read costs a lookup per chunk in the box, so one bounding box
#' around scattered chunks (a track, a few profiles far apart) could be
#' millions of lookups for a handful of chunks. A box much larger than its
#' chunks is split at the widest gap of its longest axis, until every box
#' is at most `ratio` times its chunks (or `min_box` chunks).
#' @param idx numeric matrix, one row per chunk, 0-based indices.
#' @returns list of list(lo, hi), inclusive index ranges.
#' @noRd
ic_boxes <- function(idx, ratio = 64, min_box = 4096) {
  todo <- list(idx)
  out <- list()
  while (length(todo)) {
    m <- todo[[1L]]; todo <- todo[-1L]
    lo <- apply(m, 2L, min); hi <- apply(m, 2L, max)
    ext <- hi - lo + 1
    if (nrow(m) == 1L || prod(ext) <= max(min_box, ratio * nrow(m))) {
      out <- c(out, list(list(lo = lo, hi = hi)))
      next
    }
    d <- which.max(ext)
    v <- sort(unique(m[, d]))
    cut <- v[which.max(diff(v))]                 # last value before the gap
    left <- m[, d] <= cut
    todo <- c(todo, list(m[left, , drop = FALSE], m[!left, , drop = FALSE]))
  }
  out
}

#' The array path of a chunk key ("a/b/c/0/1" -> "a/b"), NA for other keys
#' @noRd
ic_chunk_array <- function(keys) {
  is_chunk <- grepl("(^|/)c(/[0-9]+)*$", keys) | grepl("^c$", keys)
  out <- rep(NA_character_, length(keys))
  out[is_chunk] <- sub("/?c(/[0-9]+)*$", "", keys[is_chunk])
  out
}

method(store_refs, IcechunkStore) <- function(store, keys) {
  keys <- sub("^/+", "", as.character(keys))
  out <- refs_frame(keys, path = keys, transport = "store")
  arrays <- ic_chunk_array(keys)
  for (a in unique(stats::na.omit(arrays))) {
    rows <- which(arrays == a)
    refs <- ic_array_refs(store, a, keys[rows])
    m <- match(keys[rows], refs$key)
    out$transport[rows[is.na(m)]] <- "absent"
    hit <- rows[!is.na(m)]
    r <- refs[m[!is.na(m)], , drop = FALSE]
    inl <- r$kind == "inline"
    if (any(inl)) {
      out$transport[hit[inl]] <- "inline"
      out$data[hit[inl]] <- r$inline[inl]
      out$size[hit[inl]] <- lengths(r$inline[inl])
    }
    if (store@direct) {
      vir <- r$kind == "virtual" & ic_authorized(r$location, store@authorized)
      if (any(vir)) {
        loc <- r$location[vir]
        local <- grepl("^file://", loc)
        loc[local] <- file_url_path(loc[local])
        out$path[hit[vir]] <- loc
        out$offset[hit[vir]] <- as.numeric(r$offset[vir])
        out$size[hit[vir]] <- as.numeric(r$length[vir])
        out$transport[hit[vir]] <- ifelse(local, "local", url_transport(loc))
      }
    }
  }
  out
}

#' Local paths from file:// URLs (percent-decoded), Windows drive letters
#' included
#' @noRd
file_url_path <- function(x) {
  p <- vapply(sub("^file://", "", x), utils::URLdecode, "", USE.NAMES = FALSE)
  ## file:///C:/dir/f is C:/dir/f on Windows, not /C:/dir/f
  sub("^/([A-Za-z]:[/\\\\])", "\\1", p)
}

#' Locations within an authorized container prefix
#' @noRd
ic_authorized <- function(location, prefixes) {
  if (!length(prefixes)) return(rep(FALSE, length(location)))
  Reduce(`|`, lapply(prefixes, function(p) startsWith(location, p)))
}

#' Fetch "store" rows of a plan in one call: icechunk reads them concurrently
#' @noRd
ic_fetch_store_rows <- function(store, plan, rows) {
  keys <- plan$key[rows]
  tabularberg::ic_get_many(store@session, keys)
}
