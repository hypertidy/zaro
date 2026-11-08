# dataset.R -- a whole store as an inventory of arrays
#
# zaro_dataset()  every array in one group of a store (the root by default):
#                 metadata, dimension names and sizes in R order, the
#                 group's attributes
# zaro_groups()   the groups of a store, with how many arrays each holds
#
# This is the hand-off to array-semantics packages (ndr): zaro says which
# arrays exist, what their dimensions are and how to read them lazily
# (zaro_array()); what the arrays *mean* (coordinates, time, selection) is
# left to the caller.


#' Open a store as a dataset: every array, its dimensions, no data read
#'
#' Collects the metadata of every array in one group of a Zarr store (the
#' root by default) and
#' reports each array's dimension names and sizes in R order (the order
#' [zaro_array()] returns). Consolidated metadata (`.zmetadata` or a
#' consolidated `zarr.json`) is used when present, which is one request for
#' the whole store. Otherwise the store is listed and each child is probed
#' for array metadata; pass `arrays` to skip the listing (useful over plain
#' HTTP, which can't list).
#'
#' No array data is read. Use [zaro_array()] on the result to get a lazy
#' array, or [zaro_read()] with `meta = ds$arrays[[name]]`.
#'
#' Arrays with no dimension names (Zarr V2 without xarray's
#' `_ARRAY_DIMENSIONS`) get names `"<array>_dim<k>"`, with `k` the 0-based
#' Zarr axis, so they never collide with another array's dimensions.
#' Only arrays directly in `group` are included; use [zaro_groups()] to see
#' a nested store's groups (e.g. EOPF and GeoZarr products, which keep their
#' arrays several groups down).
#'
#' @param source a store from [zaro()], or a character source passed to
#'   [zaro()] along with `...`.
#' @param group path of the group to open, e.g.
#'   `"measurements/reflectance/r10m"`. Default `""`, the root.
#' @param arrays optional character vector of array names (within `group`)
#'   to include. With consolidated metadata it filters; without, it replaces
#'   the listing.
#' @param verbose logical. Emit diagnostic messages. Default
#'   `getOption("zaro.verbose", TRUE)`.
#' @param ... passed to [zaro()] when `source` is character.
#' @returns A `zaro_dataset`: a list with `store`, `group` (the group's
#'   path), `attributes` (the group's attributes), `arrays` (named list of
#'   array metadata), `paths` (named character vector: each array's path in
#'   the store), `dims` (named list of R-order dimension names per array) and
#'   `shape` (named list of R-order sizes per array). Arrays are named
#'   within the group; [zaro_array()], [zaro_read()] and [zaro_plan()] take
#'   the dataset with those names.
#'
#' @export
#' @examples
#' \dontrun{
#' ds <- zaro_dataset("s3://aodn-cloud-optimised/model_sea_level_anomaly_gridded_realtime.zarr",
#'                    anonymous = TRUE, region = "ap-southeast-2")
#' ds
#' ds$dims$GSLA                       # "LONGITUDE" "LATITUDE" "TIME"
#' gsla <- zaro_array(ds, "GSLA", cf = TRUE)
#'
#' ## a nested store: list the groups, open one
#' eopf <- zaro("https://s3.explorer.eopf.copernicus.eu/esa-zarr-sentinel-explorer-fra/tests-output/sentinel-2-l2a/S2C_MSIL2A_20260120T084301_N0511_R064_T36UXA_20260120T122910.zarr")
#' zaro_groups(eopf)
#' r10m <- zaro_dataset(eopf, group = "measurements/reflectance/r10m")
#' b04 <- zaro_array(r10m, "b04")
#' }
zaro_dataset <- function(source, group = "", arrays = NULL,
                         verbose = zaro_verbose(), ...) {
  store <- as_store(source, verbose, ...)
  group <- clean_group(group)

  root <- tryCatch(zaro_meta(store, "", verbose = verbose),
                   error = function(e) NULL)
  cm <- if (is.null(root)) NULL else attr(root, "consolidated")
  if (!nzchar(group)) {
    attributes <- if (is.null(root)) list() else root@attributes
  } else if (!is.null(cm)) {
    if (is.null(cm[[group]]) || !identical(cm[[group]]@node_type, "group")) {
      stop("group '", group, "' is not in the store; see zaro_groups()",
           call. = FALSE)
    }
    attributes <- cm[[group]]@attributes
  } else {
    gm <- tryCatch(zaro_meta(store, group, consolidated = FALSE,
                             verbose = FALSE), error = function(e) NULL)
    attributes <- if (is.null(gm)) list() else gm@attributes
  }

  if (!is.null(cm)) {
    metas <- Filter(function(m) identical(m@node_type, "array"), cm)
    metas <- children_of(metas, group)
    if (!is.null(arrays)) {
      missing <- setdiff(arrays, names(metas))
      if (length(missing)) {
        stop("array(s) not found: ", paste(missing, collapse = ", "),
             call. = FALSE)
      }
      metas <- metas[arrays]
    }
  } else {
    if (is.null(arrays)) arrays <- list_children(store, group)
    vmsg("no consolidated metadata, probing ", length(arrays), " node(s)",
         verbose = verbose)
    metas <- lapply(arrays, function(nm) {
      tryCatch(zaro_meta(store, group_path(group, nm), consolidated = FALSE,
                         verbose = FALSE),
               error = function(e) NULL)
    })
    names(metas) <- arrays
    metas <- Filter(function(m) !is.null(m) && identical(m@node_type, "array"),
                    metas)
  }

  layout <- Map(r_layout, metas, names(metas))
  ds <- structure(list(
    store      = store,
    group      = group,
    attributes = attributes,
    arrays     = metas,
    paths      = vapply(names(metas), function(nm) group_path(group, nm), ""),
    dims       = lapply(layout, `[[`, "dims"),
    shape      = lapply(layout, `[[`, "shape")
  ), class = "zaro_dataset")
  check_dim_sizes(ds)
  ds
}


#' @export
print.zaro_dataset <- function(x, ...) {
  grp <- if (nzchar(x$group %||% "")) sprintf(" group '%s'", x$group) else ""
  cat(sprintf("<zaro_dataset> %d array(s)%s [%s]\n", length(x$arrays), grp,
              class(x$store)[1]))
  if (length(x$arrays)) {
    lab <- vapply(names(x$arrays), function(nm) {
      sprintf("(%s)", paste(x$dims[[nm]], x$shape[[nm]], sep = ": ",
                            collapse = ", "))
    }, character(1))
    dt <- vapply(x$arrays, function(m) m@data_type, character(1))
    w <- max(nchar(names(x$arrays)))
    cat(sprintf("  %-*s %-8s %s\n", w, names(x$arrays), dt, lab), sep = "")
  }
  if (length(x$attributes)) {
    cat(sprintf("  attributes: %d\n", length(x$attributes)))
  }
  invisible(x)
}


#' The groups of a store
#'
#' Lists every group of a Zarr store, the root included, with the number of
#' arrays directly in each, so a nested product can be opened one group at
#' a time with `zaro_dataset(group = )`. Uses consolidated metadata when the
#' root has it (one request); otherwise lists the store, which needs a store
#' that can be listed (not plain HTTP).
#'
#' @inheritParams zaro_dataset
#' @param source a store from [zaro()], or a character source passed to
#'   [zaro()] along with `...`.
#' @returns A data frame with columns `group` (path, `""` for the root) and
#'   `arrays` (number of arrays directly in the group), sorted by path.
#' @export
#' @examples
#' \dontrun{
#' zaro_groups("https://s3.explorer.eopf.copernicus.eu/esa-zarr-sentinel-explorer-fra/tests-output/sentinel-2-l2a/S2C_MSIL2A_20260120T084301_N0511_R064_T36UXA_20260120T122910.zarr")
#' }
zaro_groups <- function(source, verbose = zaro_verbose(), ...) {
  store <- as_store(source, verbose, ...)
  root <- tryCatch(zaro_meta(store, "", verbose = verbose),
                   error = function(e) NULL)
  cm <- if (is.null(root)) NULL else attr(root, "consolidated")
  if (!is.null(cm)) {
    type <- vapply(cm, function(m) m@node_type, character(1))
    groups <- c("", names(cm)[type == "group"])
    arrays <- names(cm)[type == "array"]
  } else {
    keys <- tryCatch(store_list(store, prefix = "", recursive = TRUE),
                     error = function(e) NULL)
    if (is.null(keys)) {
      keys <- tryCatch(store_list(store, prefix = ""), error = function(e) NULL)
    }
    if (is.null(keys)) {
      stop("store has no consolidated metadata and can't be listed",
           call. = FALSE)
    }
    keys <- sub("^/+", "", keys)
    node_of <- function(pat) {
      k <- keys[grepl(pat, keys)]
      unique(sub(pat, "", k))
    }
    arrays <- c(node_of("/?(\\.zarray)$"),
                node_of("/?zarr\\.json$")[vapply(
                  node_of("/?zarr\\.json$"), function(p) {
                    m <- tryCatch(zaro_meta(store, p, consolidated = FALSE,
                                            verbose = FALSE),
                                  error = function(e) NULL)
                    !is.null(m) && identical(m@node_type, "array")
                  }, logical(1))])
    groups <- unique(c("", node_of("/?(\\.zgroup)$"),
                       setdiff(node_of("/?zarr\\.json$"), arrays)))
  }
  parent <- ifelse(grepl("/", arrays, fixed = TRUE),
                   sub("/[^/]*$", "", arrays), "")
  groups <- sort(unique(groups))
  data.frame(group = groups,
             arrays = vapply(groups, function(g) sum(parent == g), integer(1)),
             row.names = NULL, stringsAsFactors = FALSE)
}


# -- internal helpers ---------------------------------------------------------

#' A store from a store or a character source
#' @noRd
as_store <- function(source, verbose, ...) {
  store <- if (is.character(source)) zaro(source, verbose = verbose, ...) else source
  if (inherits(store, "zaro_dataset")) store <- store$store
  if (!S7::S7_inherits(store, ZaroStore)) {
    stop("'source' must be a zaro store or a character source", call. = FALSE)
  }
  store
}

#' A group path without leading/trailing slashes ("" for the root)
#' @noRd
clean_group <- function(group) {
  if (is.null(group) || length(group) != 1L || is.na(group)) {
    stop("'group' must be a single path", call. = FALSE)
  }
  group <- gsub("^/+|/+$", "", group)
  if (group == ".") "" else group
}

#' Path of a node in a group
#' @noRd
group_path <- function(group, name) {
  if (nzchar(group)) paste0(group, "/", name) else name
}

#' Entries of a consolidated listing directly in `group`, renamed relative
#' @noRd
children_of <- function(metas, group) {
  nms <- names(metas)
  if (nzchar(group)) {
    pre <- paste0(group, "/")
    keep <- startsWith(nms, pre)
    metas <- metas[keep]
    nms <- substring(nms[keep], nchar(pre) + 1L)
  }
  keep <- !grepl("/", nms, fixed = TRUE)
  stats::setNames(metas[keep], nms[keep])
}

#' Store, path and metadata of an array named in a dataset
#' @noRd
dataset_array <- function(ds, name, meta = NULL) {
  if (is.null(meta)) meta <- ds$arrays[[name]]
  if (is.null(meta)) {
    stop("array '", name, "' is not in the dataset", call. = FALSE)
  }
  path <- ds$paths[[name]] %||% name
  list(store = ds$store, path = path, meta = meta)
}

#' Dimension names and sizes in R order (reversed for C-order arrays)
#' @noRd
r_layout <- function(meta, name) {
  shape <- meta@shape
  dn <- meta@dimension_names
  if (is.null(dn) || length(dn) != length(shape)) {
    dn <- if (length(shape)) paste0(name, "_dim", seq_along(shape) - 1L) else character()
  }
  dn <- as.character(unlist(dn))
  if (identical(meta@raw_meta[["order"]] %||% "C", "C")) {
    shape <- rev(shape)
    dn <- rev(dn)
  }
  list(dims = dn, shape = stats::setNames(as.integer(shape), dn))
}


#' Names of a group's children, from a store listing
#' @noRd
list_children <- function(store, group = "") {
  prefix <- if (nzchar(group)) paste0(group, "/") else ""
  keys <- tryCatch(store_list(store, prefix = prefix), error = function(e) NULL)
  if (is.null(keys)) {
    stop("store has no consolidated metadata and can't be listed; ",
         "pass the array names with 'arrays ='", call. = FALSE)
  }
  keys <- sub("^/+", "", keys)
  if (nzchar(prefix)) {
    keys <- ifelse(startsWith(keys, prefix), substring(keys, nchar(prefix) + 1L), keys)
  }
  top <- unique(sub("/.*$", "", keys))
  top[nzchar(top) & !startsWith(top, ".") & top != "zarr.json"]
}


#' Warn when arrays disagree on the size of a shared dimension
#' @noRd
check_dim_sizes <- function(ds) {
  sizes <- unlist(unname(ds$shape))
  if (!length(sizes)) return(invisible())
  bad <- names(which(tapply(sizes, names(sizes),
                            function(s) length(unique(s)) > 1L)))
  if (length(bad)) {
    warning("arrays disagree on the size of dimension(s): ",
            paste(bad, collapse = ", "), call. = FALSE)
  }
  invisible()
}
