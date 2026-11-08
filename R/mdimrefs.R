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
# `.zmetadata` is synthesised from `arrays`, a chunk key is one lookup in
# `refs_<array>` followed by a byte-range read of (path, offset, size).
# Coordinates held inline or as start/step are served as one whole-array chunk.
# `remap` rows of the chosen target rewrite the verbatim source paths; with
# no target given, `meta.default_target` is applied (decision 0004).

MdimRefsStore <- new_class("MdimRefsStore", parent = ZaroStore, properties = list(
  con    = class_any,        # DBI connection to the SQLite store
  remap  = class_data.frame, # prefix, replacement for the chosen target
  arrays = class_data.frame, # the arrays table
  target = class_character   # the remap target applied ("" = verbatim)
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
  MdimRefsStore(root = source, con = con, remap = remap,
                arrays = DBI::dbGetQuery(con, "SELECT * FROM arrays"),
                target = target)
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

mdimrefs_meta <- function(store) {
  A <- store@arrays
  md <- list(".zgroup" = list(zarr_format = 2L), ".zattrs" = structure(list(), names = character()))
  for (i in seq_len(nrow(A))) {
    shape <- mdimrefs_json(A$shape[i])
    ref <- A$storage[i] == "referenced"
    codec <- mdimrefs_json(A$codec[i])
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
  md <- mdimrefs_meta(store)
  if (key == ".zmetadata")
    return(charToRaw(to_json_exact(list(metadata = md, zarr_consolidated_format = 1L))))
  if (key %in% names(md))
    return(charToRaw(to_json_exact(md[[key]])))

  fetch_ref(store, store_refs(store, key))
}

# A chunk key is one lookup in refs_<array>: present with a path is a byte
# range (remapped), present without one is inline data, missing or not
# present is absent. Coordinates held inline or as start/step are one
# whole-array chunk.
method(store_refs, MdimRefsStore) <- function(store, keys) {
  out <- refs_frame(keys, transport = "absent")
  for (i in seq_along(keys)) {
    key <- keys[i]
    arr <- sub("/[^/]*$", "", key); ck <- sub("^.*/", "", key)
    a <- store@arrays[store@arrays$array == arr, , drop = FALSE]
    if (!nrow(a) || !grepl("^[0-9]+(\\.[0-9]+)*$", ck)) next
    idx <- as.integer(strsplit(ck, ".", fixed = TRUE)[[1]])
    if (a$storage != "referenced") {
      if (all(idx == 0L)) {
        out$data[[i]] <- mdimrefs_coord_raw(a)
        out$transport[i] <- "inline"
      }
      next
    }
    where <- paste(sprintf("dim_%d = %d", seq_along(idx) - 1L, idx), collapse = " AND ")
    r <- DBI::dbGetQuery(store@con, sprintf(
      'SELECT present, path, "offset", size, data FROM %s WHERE %s',
      DBI::dbQuoteIdentifier(store@con, paste0("refs_", arr)), where))
    if (!nrow(r) || r$present[1] == 0L) next                 # absent: fill value
    if (is.na(r$path[1])) {                                     # inline chunk
      out$data[[i]] <- as.raw(r$data[[1]])
      out$transport[i] <- "inline"
      next
    }
    out$path[i] <- mdimrefs_path(store, r$path[1])
    out$offset[i] <- as.numeric(r$offset[1])
    out$size[i] <- as.numeric(r$size[1])
    out$transport[i] <- url_transport(out$path[i])
  }
  out
}

method(store_list, MdimRefsStore) <- function(store, prefix = "") {
  keys <- c(".zmetadata", names(mdimrefs_meta(store)))
  if (nzchar(prefix)) keys[startsWith(keys, prefix)] else keys
}

method(store_exists, MdimRefsStore) <- function(store, key) {
  !is.null(store_get(store, key))
}
