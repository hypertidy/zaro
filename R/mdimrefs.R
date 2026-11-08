# mdimrefs.R -- read an mdim-refs byte-reference table as a Zarr V2 store
#
# An mdim-refs store (format "mdim-refs/0.2") is a SQLite database of chunk
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
# `remap` rows of the chosen target rewrite the verbatim source paths.

MdimRefsStore <- new_class("MdimRefsStore", parent = ZaroStore, properties = list(
  con    = class_any,        # DBI connection to the SQLite store
  remap  = class_data.frame, # prefix, replacement for the chosen target
  arrays = class_data.frame  # the arrays table
))

#' Open an mdim-refs store
#'
#' @param source path to the SQLite file, or an http(s) URL (downloaded once
#'   to a temporary file).
#' @param target remap target to apply to source paths (e.g. "public"), or
#'   NULL for the verbatim sources.
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
  fmt <- DBI::dbGetQuery(con, "SELECT value FROM meta WHERE key = 'format_version'")$value
  if (!length(fmt) || !startsWith(fmt, "mdim-refs/"))
    stop("not an mdim-refs store: ", source, call. = FALSE)
  vmsg("opening ", fmt, " store: ", source, verbose = verbose)
  remap <- if (is.null(target)) data.frame(prefix = character(), replacement = character())
           else DBI::dbGetQuery(con, "SELECT prefix, replacement FROM remap WHERE target = ?
                                       ORDER BY length(prefix) DESC", params = list(target))
  MdimRefsStore(root = source, con = con, remap = remap,
                arrays = DBI::dbGetQuery(con, "SELECT * FROM arrays"))
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
        out$inline[[i]] <- mdimrefs_coord_raw(a)
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
      out$inline[[i]] <- as.raw(r$data[[1]])
      out$transport[i] <- "inline"
      next
    }
    out$url[i] <- mdimrefs_path(store, r$path[1])
    out$offset[i] <- as.numeric(r$offset[1])
    out$length[i] <- as.numeric(r$size[1])
    out$transport[i] <- url_transport(out$url[i])
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
