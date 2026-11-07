# dataset.R -- a whole store as an inventory of arrays
#
# zaro_dataset()  every array at the root of a store: metadata, dimension
#                 names and sizes in R order, root attributes
#
# This is the hand-off to array-semantics packages (ndr): zaro says which
# arrays exist, what their dimensions are and how to read them lazily
# (zaro_array()); what the arrays *mean* (coordinates, time, selection) is
# left to the caller.


#' Open a store as a dataset: every array, its dimensions, no data read
#'
#' Collects the metadata of every array at the root of a Zarr store and
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
#' Only arrays directly in the root group are included.
#'
#' @param source a store from [zaro()], or a character source passed to
#'   [zaro()] along with `...`.
#' @param arrays optional character vector of array names to include. With
#'   consolidated metadata it filters; without, it replaces the listing.
#' @param verbose logical. Emit diagnostic messages. Default
#'   `getOption("zaro.verbose", TRUE)`.
#' @param ... passed to [zaro()] when `source` is character.
#' @returns A `zaro_dataset`: a list with `store`, `attributes` (root group
#'   attributes), `arrays` (named list of array metadata), `dims` (named list
#'   of R-order dimension names per array) and `shape` (named list of R-order
#'   sizes per array).
#'
#' @export
#' @examples
#' \dontrun{
#' ds <- zaro_dataset("s3://aodn-cloud-optimised/model_sea_level_anomaly_gridded_realtime.zarr",
#'                    anonymous = TRUE, region = "ap-southeast-2")
#' ds
#' ds$dims$GSLA                       # "LONGITUDE" "LATITUDE" "TIME"
#' gsla <- zaro_array(ds, "GSLA", cf = TRUE)
#' }
zaro_dataset <- function(source, arrays = NULL, verbose = zaro_verbose(), ...) {
  store <- if (is.character(source)) zaro(source, verbose = verbose, ...) else source
  if (!S7::S7_inherits(store, ZaroStore)) {
    stop("'source' must be a zaro store or a character source", call. = FALSE)
  }

  root <- tryCatch(zaro_meta(store, "", verbose = verbose),
                   error = function(e) NULL)
  attributes <- if (is.null(root)) list() else root@attributes
  cm <- if (is.null(root)) NULL else attr(root, "consolidated")

  if (!is.null(cm)) {
    metas <- Filter(function(m) identical(m@node_type, "array"), cm)
    metas <- metas[!grepl("/", names(metas), fixed = TRUE)]
    if (!is.null(arrays)) {
      missing <- setdiff(arrays, names(metas))
      if (length(missing)) {
        stop("array(s) not found: ", paste(missing, collapse = ", "),
             call. = FALSE)
      }
      metas <- metas[arrays]
    }
  } else {
    if (is.null(arrays)) arrays <- list_children(store)
    vmsg("no consolidated metadata, probing ", length(arrays), " node(s)",
         verbose = verbose)
    metas <- lapply(arrays, function(nm) {
      tryCatch(zaro_meta(store, nm, consolidated = FALSE, verbose = FALSE),
               error = function(e) NULL)
    })
    names(metas) <- arrays
    metas <- Filter(function(m) !is.null(m) && identical(m@node_type, "array"),
                    metas)
  }

  layout <- Map(r_layout, metas, names(metas))
  ds <- structure(list(
    store      = store,
    attributes = attributes,
    arrays     = metas,
    dims       = lapply(layout, `[[`, "dims"),
    shape      = lapply(layout, `[[`, "shape")
  ), class = "zaro_dataset")
  check_dim_sizes(ds)
  ds
}


#' @export
print.zaro_dataset <- function(x, ...) {
  cat(sprintf("<zaro_dataset> %d array(s) [%s]\n", length(x$arrays),
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


# -- internal helpers ---------------------------------------------------------

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


#' Names of the root's children, from a store listing
#' @noRd
list_children <- function(store) {
  keys <- tryCatch(store_list(store, prefix = ""), error = function(e) NULL)
  if (is.null(keys)) {
    stop("store has no consolidated metadata and can't be listed; ",
         "pass the array names with 'arrays ='", call. = FALSE)
  }
  top <- unique(sub("/.*$", "", sub("^/+", "", keys)))
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
