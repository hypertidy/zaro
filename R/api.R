# api.R — flat public API for zaro
#
# zaro()          open a store
# zaro_list()     list contents
# zaro_meta()     inspect metadata (auto-detects V2/V3, consolidated)
# zaro_read()     read data from an array


#' Open a Zarr store
#'
#' Connects to a Zarr store at the given path or URI. Dispatches to the
#' appropriate backend based on the URI scheme:
#' \itemize{
#'   \item Local paths: Arrow LocalFileSystem
#'   \item \code{s3://}: Arrow S3FileSystem (with automatic region detection)
#'   \item \code{gs://}: Arrow GcsFileSystem
#'   \item \code{http://}, \code{https://}: gdalraster VSI (\code{/vsicurl/})
#'   \item \code{reference+json://}: Kerchunk JSON reference store
#'   \item \code{reference+parquet://}: Kerchunk Parquet reference store
#' }
#'
#' For S3 stores, if Arrow returns a 301 redirect (wrong region), zaro will
#' fall through to gdalraster's \code{/vsis3/} which handles region detection
#' automatically.
#'
#' @param source character. Path, URI, or reference store locator.
#' @param verbose logical. Emit progress and diagnostic messages (default TRUE).
#'   Suppress with \code{suppressMessages()}.
#' @param ... additional arguments passed to Arrow filesystem constructors
#'   (e.g. \code{anonymous = TRUE}, \code{endpoint_override}, \code{region}).
#' @returns A store object (internal class) for use with other zaro functions.
#'
#' @export
#' @examples
#' \dontrun{
#' # local store
#' store <- zaro("/data/my_dataset.zarr")
#'
#' # S3 store (anonymous)
#' store <- zaro("s3://mur-sst/zarr-v1", anonymous = TRUE)
#'
#' # Kerchunk JSON reference
#' store <- zaro("reference+json:///path/to/refs.json")
#'
#' # then explore
#' zaro_list(store)
#' zaro_meta(store, "analysed_sst")
#' data <- zaro_read(store, "analysed_sst",
#'                   start = c(0, 0, 0), count = c(1, 100, 100))
#' }
zaro <- function(source, verbose = TRUE, ...) {
  # Kerchunk JSON reference store
  if (grepl("^reference\\+json://", source)) {
    path <- sub("^reference\\+json://", "", source)
    vmsg("opening Kerchunk JSON reference store: ", path, verbose = verbose)
    return(parse_kerchunk_json(path))
  }

  # Kerchunk Parquet reference store
  if (grepl("^reference\\+parquet://", source)) {
    path <- sub("^reference\\+parquet://", "", source)
    vmsg("opening Kerchunk Parquet reference store: ", path, verbose = verbose)
    return(parse_kerchunk_parquet(path))
  }

  # S3 — try Arrow first, fall back to gdalraster /vsis3/ on 301
  if (grepl("^s3://", source)) {
    vmsg("opening S3 store via Arrow: ", source, verbose = verbose)
    store <- tryCatch({
      fs <- arrow::S3FileSystem$create(...)
      root <- sub("^s3://", "", source)
      s <- ArrowStore(root = root, fs = fs)
      # probe: try listing to detect region redirect early
      store_list(s, prefix = "")
      s
    }, error = function(e) {
      if (grepl("301|redirect|PermanentRedirect", conditionMessage(e), ignore.case = TRUE)) {
        vmsg("Arrow S3 returned 301 (region redirect), falling back to gdalraster /vsis3/",
             verbose = verbose)
        if (!requireNamespace("gdalraster", quietly = TRUE)) {
          stop("S3 bucket requires region-aware access. Either:\n",
               "  - specify region= in zaro(), e.g. zaro(\"", source,
               "\", region = \"ap-southeast-2\", anonymous = TRUE)\n",
               "  - install gdalraster for automatic region detection via /vsis3/",
               call. = FALSE)
        }
        # propagate anonymous flag to GDAL
        dots <- list(...)
        if (isTRUE(dots[["anonymous"]])) {
          gdalraster::set_config_option("AWS_NO_SIGN_REQUEST", "YES")
          vmsg("set AWS_NO_SIGN_REQUEST=YES for anonymous access", verbose = verbose)
        }
        s3_path <- sub("^s3://", "", source)
        vsi_root <- paste0("/vsis3/", s3_path)
        vmsg("using gdalraster VSI store: ", vsi_root, verbose = verbose)
        return(VSIStore(root = vsi_root))
      }
      stop(e)
    })
    return(store)
  }

  # GCS
  if (grepl("^gs://", source)) {
    vmsg("opening GCS store via Arrow: ", source, verbose = verbose)
    fs <- arrow::GcsFileSystem$create(...)
    root <- sub("^gs://", "", source)
    return(ArrowStore(root = root, fs = fs))
  }

  # HTTP/HTTPS — use gdalraster VSI
  if (grepl("^https?://", source)) {
    vsi_root <- paste0("/vsicurl/", source)
    vmsg("opening HTTP store via gdalraster VSI: ", vsi_root, verbose = verbose)
    return(VSIStore(root = vsi_root))
  }

  # local filesystem
  vmsg("opening local store: ", source, verbose = verbose)
  fs <- arrow::LocalFileSystem$create()
  ArrowStore(root = normalizePath(source, mustWork = FALSE), fs = fs)
}


#' List contents of a Zarr store
#'
#' @param store a store object from [zaro()]
#' @param path character. Path prefix within the store (default root).
#' @returns character vector of keys (relative paths).
#'
#' @export
zaro_list <- function(store, path = "") {
  store_list(store, prefix = path)
}


#' Get metadata for a Zarr node
#'
#' Reads and parses the metadata for an array or group at the given path.
#' Automatically detects Zarr V2 (\code{.zarray}/\code{.zattrs}/\code{.zmetadata})
#' vs V3 (\code{zarr.json}) format. For cloud stores, consolidated metadata
#' (\code{.zmetadata} for V2, consolidated \code{zarr.json} for V3) is preferred
#' to minimise the number of requests.
#'
#' @param store a store object from [zaro()]
#' @param path character. Path to the array or group within the store.
#'   Use \code{""} or \code{"/"} for the root group.
#' @param consolidated logical. If \code{TRUE}, attempt to read consolidated
#'   metadata first (default).
#' @param verbose logical. Emit diagnostic messages (default TRUE).
#' @returns A ZaroMeta object. For root group requests with consolidated
#'   metadata, the individual array metadata are attached as
#'   \code{attr(, "consolidated")}.
#'
#' @export
zaro_meta <- function(store, path = "", consolidated = TRUE, verbose = TRUE) {

  # --- consolidated metadata for root group requests ---
  if (consolidated && (path == "" || path == "/")) {
    # try V3 consolidated zarr.json
    raw <- store_get(store, "zarr.json")
    if (!is.null(raw)) {
      vmsg("found zarr.json at root (Zarr V3)", verbose = verbose)
      cm <- parse_consolidated(raw)
      if (!is.null(cm)) {
        vmsg("reading consolidated metadata (", length(cm), " entries)",
             verbose = verbose)
        root_meta <- parse_zarr_json(raw)
        attr(root_meta, "consolidated") <- cm
        return(root_meta)
      }
      return(parse_zarr_json(raw))
    }

    # try V2 consolidated .zmetadata
    raw <- store_get(store, ".zmetadata")
    if (!is.null(raw)) {
      vmsg("found .zmetadata (Zarr V2 consolidated)", verbose = verbose)
      cm <- parse_zmetadata(raw)
      if (!is.null(cm)) {
        vmsg("  ", length(cm), " arrays: ",
             paste(names(cm), collapse = ", "), verbose = verbose)
        # build root group meta from .zgroup/.zattrs in the same document
        doc <- jsonlite::fromJSON(rawToChar(raw), simplifyVector = FALSE)
        entries <- doc[["metadata"]]
        zgroup_raw <- if (".zgroup" %in% names(entries)) {
          charToRaw(jsonlite::toJSON(entries[[".zgroup"]], auto_unbox = TRUE))
        } else {
          NULL
        }
        zattrs_raw <- if (".zattrs" %in% names(entries)) {
          charToRaw(jsonlite::toJSON(entries[[".zattrs"]], auto_unbox = TRUE))
        } else {
          NULL
        }
        root_meta <- if (!is.null(zgroup_raw)) {
          parse_zgroup(zgroup_raw, zattrs_raw)
        } else {
          parse_zgroup(charToRaw('{"zarr_format": 2}'), zattrs_raw)
        }
        attr(root_meta, "consolidated") <- cm
        return(root_meta)
      }
    }
  }

  # --- consolidated lookup for specific variable paths ---
  if (consolidated && nzchar(path) && path != "/") {
    # try V2 .zmetadata (one request for all variables)
    zmeta_raw <- store_get(store, ".zmetadata")
    if (!is.null(zmeta_raw)) {
      vmsg("checking .zmetadata for '", path, "'", verbose = verbose)
      cm <- parse_zmetadata(zmeta_raw)
      if (!is.null(cm) && path %in% names(cm)) {
        vmsg("found '", path, "' in consolidated metadata", verbose = verbose)
        meta <- cm[[path]]
        report_meta(meta, verbose)
        return(meta)
      }
    }
  }

  # --- try V3: zarr.json ---
  v3_key <- if (nzchar(path) && path != "/") {
    paste0(path, "/zarr.json")
  } else {
    "zarr.json"
  }
  raw <- store_get(store, v3_key)
  if (!is.null(raw)) {
    vmsg("found ", v3_key, " (Zarr V3)", verbose = verbose)
    meta <- parse_zarr_json(raw)
    report_meta(meta, verbose)
    return(meta)
  }

  # --- fall back to V2: .zarray / .zgroup / .zattrs ---
  vmsg("no zarr.json found, trying V2 metadata (.zarray/.zgroup)",
       verbose = verbose)

  # try .zarray (array node)
  v2_array_key <- if (nzchar(path) && path != "/") {
    paste0(path, "/.zarray")
  } else {
    ".zarray"
  }
  zarray_raw <- store_get(store, v2_array_key)

  if (!is.null(zarray_raw)) {
    vmsg("found ", v2_array_key, " (Zarr V2 array)", verbose = verbose)

    # also try .zattrs
    v2_attrs_key <- if (nzchar(path) && path != "/") {
      paste0(path, "/.zattrs")
    } else {
      ".zattrs"
    }
    zattrs_raw <- store_get(store, v2_attrs_key)
    if (!is.null(zattrs_raw)) {
      vmsg("found ", v2_attrs_key, verbose = verbose)
    }

    meta <- parse_zarray(zarray_raw, zattrs_raw)
    report_meta(meta, verbose)
    return(meta)
  }

  # try .zgroup (group node)
  v2_group_key <- if (nzchar(path) && path != "/") {
    paste0(path, "/.zgroup")
  } else {
    ".zgroup"
  }
  zgroup_raw <- store_get(store, v2_group_key)

  if (!is.null(zgroup_raw)) {
    vmsg("found ", v2_group_key, " (Zarr V2 group)", verbose = verbose)

    v2_attrs_key <- if (nzchar(path) && path != "/") {
      paste0(path, "/.zattrs")
    } else {
      ".zattrs"
    }
    zattrs_raw <- store_get(store, v2_attrs_key)

    return(parse_zgroup(zgroup_raw, zattrs_raw))
  }

  stop("no Zarr metadata found at path '", path,
       "' (tried zarr.json, .zmetadata, .zarray, .zgroup)", call. = FALSE)
}


#' Read data from a Zarr array
#'
#' Reads a hyperslab from a Zarr array, fetching and decoding the necessary
#' chunks and assembling them into an R array. Supports both Zarr V2 and V3.
#'
#' @param store a store object from [zaro()]
#' @param path character. Path to the array within the store.
#' @param start integer vector. 0-based start indices for each dimension.
#'   Defaults to the origin.
#' @param count integer vector. Number of elements to read along each
#'   dimension. Defaults to the full extent.
#' @param meta optional pre-fetched ZaroMeta object. If NULL (default),
#'   metadata is read from the store.
#' @param verbose logical. Emit diagnostic messages (default TRUE).
#' @returns An R array with dimensions matching \code{count}.
#'
#' @export
zaro_read <- function(store, path, start = NULL, count = NULL, meta = NULL,
                      verbose = TRUE) {
  if (is.null(meta)) {
    meta <- zaro_meta(store, path, consolidated = TRUE, verbose = verbose)
  }
  if (meta@node_type != "array") {
    stop("path '", path, "' is a group, not an array", call. = FALSE)
  }

  ndim <- length(meta@shape)

  # default to full extent
  if (is.null(start)) start <- rep(0L, ndim)
  if (is.null(count)) count <- meta@shape - start

  start <- as.integer(start)
  count <- as.integer(count)

  if (length(start) != ndim || length(count) != ndim) {
    stop("start and count must have length ", ndim, call. = FALSE)
  }

  # compute which chunks we need
  chunk_ranges <- chunk_coverage(start, count, meta@chunk_shape)
  n_total <- prod(vapply(chunk_ranges, length, integer(1)))
  vmsg("reading ", n_total, " chunk(s) for path '", path, "'",
       " (V", meta@zarr_format, ")", verbose = verbose)

  # allocate output array filled with fill_value
  out <- array(meta@fill_value, dim = count)

  # fetch and decode each chunk, copy the relevant portion into out
  slab_end <- start + count - 1L

  for (cidx in chunk_index_iter(chunk_ranges)) {
    chunk_raw <- fetch_chunk(store, path, cidx, meta)

    if (is.null(chunk_raw)) {
      next  # missing chunk -> fill_value (already set)
    }

    values <- decode_chunk(chunk_raw, meta)

    # this chunk covers [chunk_start, chunk_end] in array coordinates
    chunk_start <- cidx * meta@chunk_shape
    chunk_end   <- pmin(chunk_start + meta@chunk_shape, meta@shape) - 1L

    # region of overlap in array coordinates
    ov_start <- pmax(chunk_start, start)
    ov_end   <- pmin(chunk_end, slab_end)

    # positions within the chunk (0-based) and output (1-based)
    in_chunk_start <- ov_start - chunk_start
    in_chunk_count <- ov_end - ov_start + 1L
    in_out_start   <- ov_start - start + 1L

    # reshape decoded values to actual chunk extent (edge chunks may be short)
    actual_chunk_shape <- pmin(meta@chunk_shape, meta@shape - chunk_start)
    dim(values) <- actual_chunk_shape

    # build index lists for source and destination
    src_idx <- lapply(seq_len(ndim), function(d) {
      seq.int(in_chunk_start[d] + 1L, in_chunk_start[d] + in_chunk_count[d])
    })
    dst_idx <- lapply(seq_len(ndim), function(d) {
      seq.int(in_out_start[d], in_out_start[d] + in_chunk_count[d] - 1L)
    })

    # extract slice from chunk
    chunk_slice <- do.call(`[`, c(list(values), src_idx, list(drop = FALSE)))

    # insert into output — use abind-style positional assignment
    out <- do.call(`[<-`, c(list(out), dst_idx, list(value = chunk_slice)))
  }

  # set dim names if available
  if (!is.null(meta@dimension_names)) {
    names(dim(out)) <- meta@dimension_names
  }

  out
}


# -- internal helpers --------------------------------------------------------

#' Report metadata details verbosely
#' @noRd
report_meta <- function(meta, verbose) {
  if (!isTRUE(verbose)) return(invisible(NULL))
  if (meta@node_type != "array") return(invisible(NULL))

  vmsg("  dtype: ", meta@data_type,
       " | shape: [", paste(meta@shape, collapse = ", "), "]",
       " | chunks: [", paste(meta@chunk_shape, collapse = ", "), "]",
       verbose = verbose)
  if (length(meta@codecs) > 0) {
    codec_names <- vapply(meta@codecs, function(c) c$name, character(1))
    vmsg("  codecs: ", paste(codec_names, collapse = " -> "),
         verbose = verbose)
  }
  if (!is.null(meta@dimension_names)) {
    vmsg("  dimensions: ", paste(meta@dimension_names, collapse = ", "),
         verbose = verbose)
  }
  invisible(NULL)
}


#' Fetch a single chunk's raw bytes from the store
#' @noRd
fetch_chunk <- function(store, path, chunk_idx, meta) {
  key <- paste0(path, "/", chunk_key(meta, chunk_idx))
  store_get(store, key)
}


#' Compute which chunks a hyperslab intersects
#'
#' @returns list of length ndim, each element an integer vector of
#'   chunk indices (0-based) along that dimension.
#' @noRd
chunk_coverage <- function(start, count, chunk_shape) {
  end <- start + count - 1L
  lapply(seq_along(start), function(d) {
    first_chunk <- start[d] %/% chunk_shape[d]
    last_chunk  <- end[d] %/% chunk_shape[d]
    seq.int(first_chunk, last_chunk)
  })
}


#' Iterate over all chunk indices in the coverage
#'
#' @param ranges list of integer vectors from chunk_coverage()
#' @returns list of integer vectors, each a chunk multi-index
#' @noRd
chunk_index_iter <- function(ranges) {
  grid <- expand.grid(ranges, KEEP.OUT.ATTRS = FALSE)
  lapply(seq_len(nrow(grid)), function(i) as.integer(grid[i, ]))
}
