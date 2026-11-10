# icechunk.R - Icechunk repositories as a zaro store, through icechunkr
#
# An Icechunk session is a key-value view of one snapshot, so it is a Zarr
# store: metadata and chunks are read by key with icechunkr::ic_get(). What
# makes it fit zaro is the manifest: icechunkr::ic_chunk_refs() says where
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
# authorisation and its etag/last-modified checks. direct = TRUE lets zaro
# fetch virtual chunks itself (one curl pool, merged byte ranges), but only
# from containers the repository has authorised, and without the checks.
#
# A read-only session is pinned to one snapshot, so a lazy zaro_array() made
# from it keeps reading the same bytes when the branch moves on.

IcechunkStore <- new_class("IcechunkStore", parent = ZaroStore, properties = list(
  session = class_any,                 # icechunkr icechunk_session
  snapshot = class_character,          # snapshot id the session is pinned to
  authorized = class_character,        # container prefixes zaro may read directly
  direct = new_property(class_logical, default = FALSE),
  cache = class_environment            # per-array chunk refs, by array path
))


#' Open an Icechunk repository as a zaro store
#'
#' Reads one snapshot of an [Icechunk](https://icechunk.io) repository
#' through the icechunkr package. Everything zaro does with a store works on
#' the result: [zaro_meta()], [zaro_read()], [zaro_array()],
#' [zaro_dataset()], [zaro_plan()].
#'
#' The session is read-only and pinned to one snapshot (its id is in
#' `store@snapshot`), so lazy arrays made from it keep reading the same
#' version after the branch moves on. Pick the version with `branch`, `tag`
#' or `snapshot` (default branch `"main"`).
#'
#' Chunks are read through Icechunk by default, which applies the
#' repository's container authorisation and checksums. With `direct = TRUE`
#' zaro fetches virtual chunks itself, concurrently and with adjacent byte
#' ranges merged, but only from containers the repository object has
#' authorised (see `icechunkr::ic_repo_containers()`), and without
#' Icechunk's etag / last-modified checks. Native and inline chunks always
#' come from Icechunk.
#'
#' @param source an `icechunk_repo` or `icechunk_session` from icechunkr,
#'   or a repository location for `icechunkr::ic_repo_open()` (a local path,
#'   an `http(s)://` URL, or an `icechunk_storage`).
#' @param branch,tag,snapshot the version to read (one of them; default
#'   branch `"main"`). Ignored when `source` is a session.
#' @param authorize virtual chunk containers to authorise, passed to
#'   `icechunkr::ic_repo_open()` when `source` is a location.
#' @param direct logical. Fetch virtual chunks with zaro's own fetcher
#'   instead of through Icechunk (see Details).
#' @param verbose logical. Emit diagnostic messages.
#' @returns An `IcechunkStore`.
#' @export
#' @examples
#' \dontrun{
#' ex <- icechunkr::ic_example_repo()
#' store <- zaro_icechunk(ex$path, tag = "v1", authorize = ex$authorize)
#' x <- zaro_array(store, "from_bin")
#' x[1:4, 1:4]
#' zaro_dataset(store)
#' }
zaro_icechunk <- function(source, branch = NULL, tag = NULL, snapshot = NULL,
                          authorize = NULL, direct = FALSE,
                          verbose = zaro_verbose()) {
  if (!requireNamespace("icechunkr", quietly = TRUE)) {
    stop("Icechunk repositories need the icechunkr package", call. = FALSE)
  }
  repo <- NULL
  if (inherits(source, "icechunk_session")) {
    session <- source
  } else {
    repo <- if (inherits(source, "icechunk_repo")) source else
      icechunkr::ic_repo_open(source, authorize = authorize)
    if (is.null(branch) && is.null(tag) && is.null(snapshot)) branch <- "main"
    session <- icechunkr::ic_session_readonly(repo, branch = branch, tag = tag,
                                              snapshot = snapshot)
  }
  info <- icechunkr::ic_session_info(session)
  if (!isTRUE(info$read_only)) {
    stop("zaro reads Icechunk through a read-only session, so a lazy array ",
         "stays on one snapshot; use icechunkr::ic_session_readonly()",
         call. = FALSE)
  }
  authorized <- character()
  if (isTRUE(direct)) {
    if (is.null(repo)) {
      stop("direct = TRUE needs the repository (to know which containers ",
           "are authorised); pass the icechunk_repo, not a session",
           call. = FALSE)
    }
    ct <- icechunkr::ic_repo_containers(repo)
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
  icechunkr::ic_get(store@session, sub("^/+", "", key))
}

method(store_range, IcechunkStore) <- function(store, key, offset, length) {
  key <- sub("^/+", "", key)
  if (offset < 0) {
    raw <- icechunkr::ic_get(store@session, key)
    if (is.null(raw)) return(NULL)
    return(raw[seq.int(base::length(raw) - length + 1, length.out = length)])
  }
  icechunkr::ic_get(store@session, key, offset = offset, length = length)
}

method(store_exists, IcechunkStore) <- function(store, key) {
  isTRUE(icechunkr::ic_exists(store@session, sub("^/+", "", key)))
}

# Icechunk lists node prefixes only (a group or an array path, not "x/c")
method(store_list, IcechunkStore) <- function(store, prefix = "", recursive = FALSE) {
  prefix <- sub("^/+", "", sub("/+$", "", prefix))
  keys <- icechunkr::ic_list(store@session, prefix = prefix,
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

#' The chunk refs of one array, from the manifest, cached on the store
#' @noRd
ic_array_refs <- function(store, array) {
  hit <- store@cache[[array]]
  if (!is.null(hit)) return(hit)
  refs <- icechunkr::ic_chunk_refs(store@session, path = paste0("/", array),
                                   inline = TRUE)
  refs$key <- sub("^/+", "", refs$key)
  assign(array, refs, envir = store@cache)
  refs
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
    refs <- ic_array_refs(store, a)
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
        loc[local] <- utils::URLdecode(sub("^file://", "", loc[local]))
        out$path[hit[vir]] <- loc
        out$offset[hit[vir]] <- as.numeric(r$offset[vir])
        out$size[hit[vir]] <- as.numeric(r$length[vir])
        out$transport[hit[vir]] <- ifelse(local, "local", url_transport(loc))
      }
    }
  }
  out
}

#' Locations within an authorised container prefix
#' @noRd
ic_authorized <- function(location, prefixes) {
  if (!length(prefixes)) return(rep(FALSE, length(location)))
  Reduce(`|`, lapply(prefixes, function(p) startsWith(location, p)))
}

#' Fetch "store" rows of a plan in one call: icechunk reads them concurrently
#' @noRd
ic_fetch_store_rows <- function(store, plan, rows) {
  keys <- plan$key[rows]
  icechunkr::ic_get_many(store@session, keys)
}
