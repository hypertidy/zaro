# flatrefs.R -- read a flat refs store (the byterefs Parquet export)
#
# A flat refs store is a directory (local or remote):
#
#   arrays.json             format_version, meta, dimensions, arrays (the
#                           byterefs `arrays` table as JSON), remap
#   refs_<array>.parquet    one row per chunk: dim_0 .. dim_{n-1}, present,
#                           path, offset, size, info, data
#   sources.parquet         the sources (not needed to read)
#
# It is the published form of a byterefs store (byterefs-0.1, open issue 5:
# "rows should be sorted by the dims and split into row groups along dim_0
# so readers can prune"), written by raad.virtualize (mr_write_flat). A
# table can be hundreds of MB (BRAN2023 temp: 37.5M rows), so chunk keys are
# looked up by reading only the row groups whose dim statistics can hold
# them (parquet-ranged.R), a few at a time, with the latest kept in memory.
#
# Everything else is the byterefs store's: the arrays become synthesised
# Zarr V2 metadata, codec chains zaro doesn't know refuse their array
# (decision 0007), coordinates held inline or as start/step are one chunk,
# and `remap` rules of the chosen target rewrite source paths (decision
# 0004).

FlatRefsStore <- new_class("FlatRefsStore", parent = MdimRefsStore)

#' Open a flat refs store
#'
#' @param source the store's directory (local path or URL), or its
#'   `arrays.json`.
#' @param target remap target, as for byterefs stores.
#' @noRd
open_flatrefs <- function(source, target = NULL, verbose = zaro_verbose()) {
  base <- sub("/+$", "", sub("/arrays\\.json$", "", source))
  aj <- paste0(base, "/arrays.json")
  raw <- if (grepl("^(https?|s3|gs)://", aj)) {
    pq_ranges(aj, 0, NA)[[1L]]
  } else if (file.exists(aj)) readBin(aj, "raw", file.size(aj))
  if (is.null(raw)) stop("no arrays.json in flat refs store: ", base, call. = FALSE)
  j <- jsonlite::fromJSON(rawToChar(raw), simplifyVector = FALSE)
  fmt <- j$format_version %||% j$meta$format_version
  if (!mdimrefs_format_ok(fmt)) {
    stop("not a byterefs flat store (format_version ",
         if (length(fmt)) fmt else "missing", "; zaro reads ",
         paste(MR_FORMATS, collapse = " and "), "): ", base, call. = FALSE)
  }

  A <- flatrefs_arrays(j$arrays)
  codecs <- mdimrefs_codecs(A)
  refused <- unlist(lapply(codecs, function(x) if (is.character(x)) x))
  if (length(refused)) {
    warning("cannot read ", length(refused), " array(s) of this store: ",
            paste0(names(refused), ": ", refused, collapse = "; "), call. = FALSE)
  }

  # decision 0004: no target given means the store's default target
  if (is.null(target)) {
    target <- j$meta$default_target %||% ""
    if (nzchar(target)) {
      vmsg("applying the store's default target '", target, "'", verbose = verbose)
    }
  }
  rules <- Filter(function(r) identical(r$target, target), j$remap %||% list())
  remap <- data.frame(prefix = vapply(rules, `[[`, "", "prefix"),
                      replacement = vapply(rules, `[[`, "", "replacement"),
                      stringsAsFactors = FALSE)
  remap <- remap[order(-nchar(remap$prefix)), , drop = FALSE]
  if (nzchar(target) && !nrow(remap)) {
    warning("no remap rules for target '", target, "'; source paths are used verbatim",
            call. = FALSE)
  }
  vmsg("opening flat refs store: ", base, " (", nrow(A), " arrays)", verbose = verbose)
  FlatRefsStore(root = base, con = NULL, remap = remap, arrays = A,
                target = target, md = mdimrefs_meta(A, codecs),
                refused = if (length(refused)) refused else
                  stats::setNames(character(), character()),
                cache = new.env(parent = emptyenv()))
}

#' arrays.json entries as the byterefs `arrays` table (JSON text columns)
#' @noRd
flatrefs_arrays <- function(arrays) {
  js <- function(x) if (is.null(x)) NA_character_ else
    as.character(jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA, null = "null"))
  A <- data.frame(
    array = vapply(arrays, function(a) a$array, ""),
    kind = vapply(arrays, function(a) a$kind %||% NA_character_, ""),
    storage = vapply(arrays, function(a) a$storage, ""),
    dims = vapply(arrays, function(a) js(a$dims), ""),
    shape = vapply(arrays, function(a) js(a$shape), ""),
    chunks = vapply(arrays, function(a) js(a$chunks), ""),
    dtype = vapply(arrays, function(a) a$dtype, ""),
    fill = vapply(arrays, function(a) if (is.null(a$fill)) NA_character_ else
      as.character(a$fill), ""),
    codec = vapply(arrays, function(a) js(a$codec), ""),
    attrs = vapply(arrays, function(a) js(a$attrs), ""),
    affine = vapply(arrays, function(a) js(a$affine), ""),
    stringsAsFactors = FALSE)
  # inline coordinate values as bytes of the array's dtype, as in SQLite
  A$values <- lapply(arrays, function(a) {
    if (is.null(a$values)) return(NULL)
    v <- as.numeric(unlist(a$values))
    sz <- as.integer(sub("^.{2}", "", a$dtype))
    writeBin(if (substr(a$dtype, 2, 2) == "f") v else as.integer(v), raw(), size = sz,
             endian = if (startsWith(a$dtype, ">")) "big" else "little")
  })
  A
}

#' A referenced array's Parquet footer, read once
#' @noRd
flatrefs_footer <- function(store, a_name) {
  key <- paste0("pf:", a_name)
  pf <- store@cache[[key]]
  if (is.null(pf)) {
    pf <- pq_footer(paste0(store@root, "/refs_", a_name, ".parquet"), store)
    rank <- length(mdimrefs_json(store@arrays$shape[store@arrays$array == a_name]))
    pf$stats <- lapply(paste0("dim_", seq_len(rank) - 1L), function(d) pq_int_stats(pf, d))
    assign(key, pf, envir = store@cache)
  }
  pf
}

#' The rows of some row groups, from the cache or read (and cached)
#'
#' Each row group is kept as (key, present, path, offset, size, data), key
#' the chunk's position in the chunk grid as one number. The most recent
#' `getOption("zaro.flat_cache_row_groups", 4)` row groups are kept.
#' @noRd
flatrefs_row_groups <- function(store, a_name, pf, ids, grid) {
  ck <- function(id) sprintf("rg:%s:%d", a_name, id)
  missing <- ids[!vapply(ids, function(id) exists(ck(id), envir = store@cache,
                                                  inherits = FALSE), NA)]
  if (length(missing)) {
    rank <- length(grid)
    cols <- intersect(c(paste0("dim_", seq_len(rank) - 1L), "present", "path",
                        "offset", "size", "data"), pf$meta$schema$name)
    mult <- rev(cumprod(rev(c(grid[-1L], 1))))     # C order: last dim fastest
    # fetch together, decode one at a time (one row group's table in memory)
    raws <- pq_fetch_row_groups(pf, missing, store)
    bytes <- 0
    for (k in seq_along(missing)) {
      t <- pq_decode_row_groups(pf, missing[k], raws[k], col_select = cols)
      raws[k] <- list(NULL)
      bytes <- bytes + attr(t, "bytes")
      key <- 0
      for (d in seq_len(rank)) key <- key + as.numeric(t[[paste0("dim_", d - 1L)]]) * mult[d]
      # inline data is rare: keep only the rows that have it
      dat <- if ("data" %in% names(t)) t$data else list()
      has <- which(lengths(dat) > 0L)
      assign(ck(missing[k]), list(
        key = key, present = as.integer(t$present), path = as.character(t$path),
        offset = as.numeric(t$offset), size = as.numeric(t$size),
        data_rows = has, data = dat[has]), envir = store@cache)
      rm(t, key, dat)
    }
    vmsg("read ", length(missing), " row group(s) of refs_", a_name, ".parquet, ",
         format_bytes(bytes), verbose = getOption("zaro.verbose", FALSE))
  }
  # keep the latest few
  lru <- c(setdiff(store@cache[["lru"]] %||% character(), ck(ids)), ck(ids))
  keep_n <- getOption("zaro.flat_cache_row_groups", 4L)
  if (length(lru) > keep_n) {
    drop <- utils::head(lru, length(lru) - keep_n)
    drop <- setdiff(drop, ck(ids))
    rm(list = intersect(drop, ls(store@cache)), envir = store@cache)
    lru <- setdiff(lru, drop)
  }
  assign("lru", lru, envir = store@cache)
  lapply(ids, function(id) store@cache[[ck(id)]])
}

method(store_refs, FlatRefsStore) <- function(store, keys) {
  out <- refs_frame(keys, transport = "absent")
  arr <- sub("/[^/]*$", "", keys); ck <- sub("^.*/", "", keys)
  ok <- grepl("^[0-9]+(\\.[0-9]+)*$", ck) & arr != keys
  for (a_name in unique(arr[ok])) {
    a <- store@arrays[store@arrays$array == a_name, , drop = FALSE]
    if (!nrow(a)) next
    shape <- unlist(mdimrefs_json(a$shape))
    rank <- length(shape)
    parts <- strsplit(ck, ".", fixed = TRUE)
    w <- which(ok & arr == a_name & lengths(parts) == rank)
    if (!length(w)) next
    idx <- matrix(as.integer(unlist(parts[w])), ncol = rank, byrow = TRUE)
    if (a$storage != "referenced") {
      for (i in w[rowSums(idx != 0L) == 0L]) {
        out$data[[i]] <- mdimrefs_coord_raw(a)
        out$transport[i] <- "inline"
      }
      next
    }
    if (a_name %in% names(store@refused))
      stop("cannot read array '", a_name, "': ", store@refused[[a_name]], call. = FALSE)

    grid <- ceiling(shape / unlist(mdimrefs_json(a$chunks)))
    pf <- flatrefs_footer(store, a_name)
    # row groups whose dim ranges could hold one of the keys (no
    # statistics: every row group)
    ids <- pf$rg$id[vapply(pf$rg$id, function(id) {
      inside <- rep(TRUE, nrow(idx))
      for (k in seq_len(rank)) {
        s <- pf$stats[[k]]; s <- s[s$id == id, ]
        if (nrow(s) && !is.na(s$min) && !is.na(s$max)) {
          inside <- inside & idx[, k] >= s$min & idx[, k] <= s$max
        }
      }
      any(inside)
    }, NA)]
    if (!length(ids)) next
    mult <- rev(cumprod(rev(c(grid[-1L], 1))))
    want <- as.numeric(idx %*% mult)
    # a few row groups at a time (fetched together), so that a selection
    # spanning the whole table (a point time series) holds no more than the
    # cache size in memory
    batch <- max(1L, getOption("zaro.flat_cache_row_groups", 4L))
    for (b in split(ids, (seq_along(ids) - 1L) %/% batch)) {
      rgs <- flatrefs_row_groups(store, a_name, pf, b, grid)
      for (g in rgs) {
        m <- match(want, g$key)
        hit <- which(!is.na(m) & out$transport[w] == "absent")
        if (!length(hit)) next
        r <- m[hit]
        pres <- g$present[r] != 0L
        hit <- hit[pres]; r <- r[pres]
        if (!length(hit)) next
        i <- w[hit]
        inl <- is.na(g$path[r]) | !nzchar(g$path[r])
        for (j in which(inl)) {
          d <- g$data[match(r[j], g$data_rows)]
          if (length(d) && length(d[[1L]])) {
            out$data[[i[j]]] <- as.raw(d[[1L]])
            out$transport[i[j]] <- "inline"
          }
        }
        ref <- !inl
        bad <- ref & is.na(g$size[r]) & g$offset[r] != 0
        if (any(bad)) {
          stop("invalid chunk row for '", keys[i[which(bad)[1L]]], "': size is NULL ",
               "with offset ", g$offset[r][which(bad)[1L]], " (byterefs decision 0008)",
               call. = FALSE)
        }
        if (any(ref)) {
          p <- vapply(g$path[r][ref], function(x) mdimrefs_path(store, x), "",
                      USE.NAMES = FALSE)
          out$path[i[ref]] <- p
          out$offset[i[ref]] <- g$offset[r][ref]
          out$size[i[ref]] <- g$size[r][ref]
          out$transport[i[ref]] <- url_transport(p)
        }
      }
    }
  }
  out
}
