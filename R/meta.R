# meta.R - Zarr metadata parsing (V2 and V3)
#
# V3: metadata lives in zarr.json files at each node.
# V2: metadata lives in .zarray (array config), .zattrs (user attributes),
#     .zgroup (group marker).
#
# Both versions are normalised to ZaroMeta objects.
# V2 numpy dtype strings are mapped to V3-style data_type names.
# V2 compressor + filters are mapped to a V3-style codec list.

ZaroMeta <- new_class("ZaroMeta", properties = list(
  zarr_format  = class_integer,
  node_type    = class_character,     # "array" or "group"
  shape        = class_integer,       # integer vector (arrays only)
  data_type    = class_character,     # e.g. "float32", "int16"
  chunk_shape  = class_integer,       # integer vector
  codecs       = class_list,          # list of codec specs
  fill_value   = class_any,           # scalar
  dimension_names = class_any,        # character vector or NULL
  attributes   = class_list,          # user attributes
  chunk_key_sep = class_character,    # "/" (V3 default) or "."
  raw_meta     = class_list           # full parsed JSON for passthrough
))

#' Parse a zarr.json metadata document
#' @param raw_bytes raw vector containing zarr.json content
#' @returns ZaroMeta object
#' @noRd
parse_zarr_json <- function(raw_bytes) {
  txt <- rawToChar(raw_bytes)
  txt <- sanitize_json(txt)
  meta <- jsonlite::fromJSON(txt, simplifyVector = FALSE)

  node_type <- meta[["node_type"]] %||% "array"
  zarr_format <- as.integer(meta[["zarr_format"]] %||% 3L)

  if (node_type == "group") {
    return(ZaroMeta(
      zarr_format = zarr_format,
      node_type = "group",
      shape = integer(0),
      data_type = "",
      chunk_shape = integer(0),
      codecs = list(),
      fill_value = NULL,
      dimension_names = NULL,
      attributes = as.list(meta[["attributes"]] %||% list()),
      chunk_key_sep = "/",
      raw_meta = as.list(meta)
    ))
  }

  # array metadata
  shape <- as.integer(unlist(meta[["shape"]]))
  # a V3 data_type may be an object {"name": ..., "configuration": ...}
  if (is.list(meta[["data_type"]])) {
    meta[["data_type_configuration"]] <- meta[["data_type"]][["configuration"]]
    meta[["data_type"]] <- meta[["data_type"]][["name"]]
  }
  meta[["data_type"]] <- normalise_string_dtype(meta[["data_type"]])

  chunk_grid <- meta[["chunk_grid"]]
  chunk_shape <- as.integer(unlist(chunk_grid[["configuration"]][["chunk_shape"]]))

  chunk_key_enc <- meta[["chunk_key_encoding"]]
  # default separator: "/" for the "default" encoding, "." for "v2"
  chunk_key_sep <- chunk_key_enc[["configuration"]][["separator"]] %||%
    if (identical(chunk_key_enc[["name"]], "v2")) "." else "/"

  codecs <- parse_codecs(meta[["codecs"]])

  # sharding_indexed: present the inner chunks as the array's chunks (shape
  # and codecs), and keep the shard layout as attribute "shard" for planning
  shard <- NULL
  is_shard <- vapply(codecs, function(c) identical(c$name, "sharding_indexed"),
                     logical(1))
  if (any(is_shard)) {
    if (!is_shard[1L]) {
      stop("sharding_indexed after other codecs is not supported", call. = FALSE)
    }
    cfg <- codecs[[1L]]$configuration
    inner_codecs <- parse_codecs(cfg[["codecs"]])
    if (any(vapply(inner_codecs, function(c) identical(c$name, "sharding_indexed"),
                   logical(1)))) {
      stop("nested sharding is not supported", call. = FALSE)
    }
    inner <- as.integer(unlist(cfg[["chunk_shape"]]))
    if (any(chunk_shape %% inner != 0L)) {
      stop("shard shape [", paste(chunk_shape, collapse = ", "),
           "] is not a multiple of the inner chunk shape [",
           paste(inner, collapse = ", "), "]", call. = FALSE)
    }
    # codecs after sharding_indexed apply to the whole shard object
    # (bytes-to-bytes: crc32c, or a compressor)
    shard <- list(shape = chunk_shape,
                  index_codecs = parse_codecs(cfg[["index_codecs"]]),
                  index_location = cfg[["index_location"]] %||% "end",
                  outer_codecs = codecs[-1L])
    chunk_shape <- inner
    codecs <- inner_codecs
  }

  dim_names <- meta[["dimension_names"]]
  if (!is.null(dim_names)) {
    dim_names <- as.character(unlist(dim_names))
    if (all(dim_names == "")) dim_names <- NULL
  }

  out <- ZaroMeta(
    zarr_format = zarr_format,
    node_type = "array",
    shape = shape,
    data_type = meta[["data_type"]],
    chunk_shape = chunk_shape,
    codecs = codecs,
    fill_value = coerce_fill_value(meta[["fill_value"]], meta[["data_type"]]),
    dimension_names = dim_names,
    attributes = as.list(meta[["attributes"]] %||% list()),
    chunk_key_sep = chunk_key_sep,
    raw_meta = as.list(meta)
  )
  if (!is.null(shard)) attr(out, "shard") <- shard
  out
}


#' Parse V3 codec chain into a structured list
#' @noRd
parse_codecs <- function(codec_list) {
  if (is.null(codec_list)) return(list())

  # jsonlite may return a data.frame or a list depending on structure
  if (is.data.frame(codec_list)) {
    return(lapply(seq_len(nrow(codec_list)), function(i) {
      list(
        name = codec_list[["name"]][i],
        configuration = as.list(codec_list[["configuration"]][i, , drop = FALSE])
      )
    }))
  }

  # list of lists
  lapply(codec_list, function(codec) {
    list(
      name = codec[["name"]],
      configuration = as.list(codec[["configuration"]] %||% list())
    )
  })
}


#' Compute the store key for a chunk at a given grid index
#'
#' V3: "c/0/1/2" (with configurable separator)
#' V2: "0.1.2" (dot-separated, no "c/" prefix)
#'
#' @param meta ZaroMeta for the array
#' @param idx integer vector of chunk grid indices (0-based)
#' @returns character key suffix e.g. "c/0/1/2" (V3) or "0.1.2" (V2)
#' @noRd
chunk_key <- function(meta, idx) {
  sep <- meta@chunk_key_sep
  if (meta@zarr_format == 2L) return(paste(idx, collapse = sep))
  enc <- meta@raw_meta[["chunk_key_encoding"]][["name"]] %||% "default"
  if (identical(enc, "v2")) return(paste(idx, collapse = sep))
  # V3 default encoding: "c" then the indices, all joined by the separator
  paste(c("c", idx), collapse = sep)
}


#' Compute the number of chunks along each dimension
#' @noRd
n_chunks <- function(meta) {
  ceiling(meta@shape / meta@chunk_shape)
}


#' Parse consolidated metadata from a root zarr.json
#'
#' Returns a named list of ZaroMeta objects keyed by path.
#' @noRd
parse_consolidated <- function(raw_bytes) {
  txt <- rawToChar(raw_bytes)
  txt <- sanitize_json(txt)
  meta <- jsonlite::fromJSON(txt, simplifyVector = FALSE)

  cm <- meta[["consolidated_metadata"]]
  if (is.null(cm)) return(NULL)

  entries <- cm[["metadata"]]
  if (is.null(entries)) return(NULL)

  result <- list()
  for (path in names(entries)) {
    entry_json <- to_json_exact(entries[[path]])
    result[[path]] <- parse_zarr_json(charToRaw(entry_json))
  }
  result
}


# -- dtype helpers -----------------------------------------------------------

#' Map Zarr V3 data_type to R type info
#'
#' @param dtype character, e.g. "float32", "int16", "uint8"
#' @returns list with what (for readBin), size, signed
#' @noRd
dtype_info <- function(dtype) {
  if (dtype == "<i2") {
    dtype <- "int16"
  }
  switch(dtype,
    "bool"    = list(what = "logical", size = 1L, signed = FALSE),
    "int8"    = list(what = "integer", size = 1L, signed = TRUE),
    "int16"   = list(what = "integer", size = 2L, signed = TRUE),
    "int32"   = list(what = "integer", size = 4L, signed = TRUE),
    "int64"   = list(what = "double",  size = 8L, signed = TRUE),
    "uint8"   = list(what = "integer", size = 1L, signed = FALSE),
    "uint16"  = list(what = "integer", size = 2L, signed = FALSE),
    "uint32"  = list(what = "double",  size = 4L, signed = FALSE),
    "uint64"  = list(what = "double",  size = 8L, signed = FALSE),
    "float16" = list(what = "double",  size = 2L, signed = TRUE),
    "float32" = list(what = "double",  size = 4L, signed = TRUE),
    "float64" = list(what = "double",  size = 8L, signed = TRUE),
    # strings: the element size is in the metadata (see string_spec())
    "fixed_length_utf32" = ,
    "null_terminated_bytes" = ,
    "string"  = list(what = "character", size = NA_integer_, signed = FALSE),
    stop("unsupported dtype: ", dtype, call. = FALSE)
  )
}

#' Is a data_type one of the string types?
#' @noRd
is_string_dtype <- function(dtype) {
  dtype %in% c("fixed_length_utf32", "null_terminated_bytes", "string")
}

#' V3 string data_type names (and aliases) as zaro's three
#'
#' `fixed_length_utf32` (numpy `U`), `null_terminated_bytes` (numpy `S`;
#' `fixed_length_ascii` is an older name), and `string` for variable-length
#' UTF-8 (`variable_length_utf8` is another name).
#' @noRd
normalise_string_dtype <- function(dtype) {
  if (!is.character(dtype) || length(dtype) != 1L) return(dtype)
  switch(dtype,
    fixed_length_ascii = "null_terminated_bytes",
    variable_length_utf8 = , str = "string",
    dtype)
}

#' How the strings of an array are stored
#'
#' @returns list(kind, length_bytes, endian): `kind` is "utf32", "bytes" or
#'   "vlen"; `length_bytes` the fixed element size (NA for vlen).
#' @noRd
string_spec <- function(meta) {
  dt <- meta@data_type
  rm <- meta@raw_meta
  kind <- switch(dt, fixed_length_utf32 = "utf32",
                 null_terminated_bytes = "bytes", string = "vlen")
  if (kind == "vlen") {
    return(list(kind = kind, length_bytes = NA_integer_, endian = "little"))
  }
  if (identical(meta@zarr_format, 2L)) {
    # numpy dtype: "<U10" is 10 code points (40 bytes), "|S10" 10 bytes
    d <- rm[["dtype"]]
    n <- as.integer(sub("^[<>|=]?[US]", "", d))
    endian <- if (startsWith(d, ">")) "big" else "little"
    lb <- if (kind == "utf32") 4L * n else n
  } else {
    cfg <- rm[["data_type_configuration"]] %||% list()
    lb <- cfg[["length_bytes"]] %||%
      (if (!is.null(cfg[["length_bits"]])) cfg[["length_bits"]] %/% 8L)
    endian <- chunk_endian(meta@codecs)
  }
  if (is.null(lb) || is.na(lb)) {
    stop("no element length for string dtype ", dt, call. = FALSE)
  }
  list(kind = kind, length_bytes = as.integer(lb), endian = endian)
}

#' Number of bytes per element for a dtype
#' @noRd
dtype_size <- function(dtype) {
  dtype_info(dtype)$size
}

#' Coerce fill_value to the appropriate R type
#'
#' A Zarr fill_value may be absent (JSON `null`). After a JSON round-trip
#' (e.g. the toJSON/fromJSON pass in `parse_zmetadata()`) a `null` can also
#' degrade to an empty object `{}`, which parses back as an empty `list()`.
#' Treat all of these "no fill value" forms as missing (`NA_real_`) so the
#' output array is a numeric array rather than a list-array.
#' @noRd
coerce_fill_value <- function(fv, data_type) {
  if (is_string_dtype(normalise_string_dtype(data_type %||% ""))) {
    # strings: a JSON string is the fill (often ""), anything else missing
    if (is.character(fv) && length(fv) == 1L && !is.na(fv)) return(fv)
    return(NA_character_)
  }
  # null, empty, or a (degraded) list/object -> missing
  if (is.null(fv) || length(fv) == 0L || is.list(fv)) return(NA_real_)
  if (is.numeric(fv)) return(fv)
  if (is.character(fv)) {
    if (tolower(fv) == "nan") return(NaN)
    if (tolower(fv) == "infinity" || fv == "Inf") return(Inf)
    if (tolower(fv) == "-infinity" || fv == "-Inf") return(-Inf)
    # numeric string from JSON
    if (grepl("^-?[0-9]", fv)) return(as.numeric(fv))
  }
  if (is.logical(fv) && is.na(fv)) return(NA_real_)
  fv
}

# -- V2 metadata parsing -----------------------------------------------------

#' Parse a V2 .zarray metadata document
#'
#' Maps V2 fields to ZaroMeta. The V2 dtype (numpy string) is normalised to
#' V3-style names. The V2 compressor + filters are mapped to a codec list.
#'
#' @param zarray_bytes raw vector containing .zarray content
#' @param zattrs_bytes raw vector containing .zattrs content (or NULL)
#' @returns ZaroMeta object
#' @noRd
parse_zarray <- function(zarray_bytes, zattrs_bytes = NULL) {
  txt <- rawToChar(zarray_bytes)
  txt <- sanitize_json(txt)
  meta <- jsonlite::fromJSON(txt, simplifyVector = FALSE)

  shape <- as.integer(unlist(meta[["shape"]]))
  chunk_shape <- as.integer(unlist(meta[["chunks"]]))
  # parse numpy dtype string
  dtype_raw <- meta[["dtype"]]
  dtype_parsed <- parse_numpy_dtype(dtype_raw)
  fill_value <- coerce_fill_value(meta[["fill_value"]], dtype_parsed$dtype)

  # build codec list from V2 compressor + filters
  codecs <- v2_codecs(meta[["compressor"]], meta[["filters"]], dtype_parsed$endian)

  # parse attributes if available
  attrs <- list()
  dim_names <- NULL
  if (!is.null(zattrs_bytes)) {
    attrs <- jsonlite::fromJSON(sanitize_json(rawToChar(zattrs_bytes)), simplifyVector = FALSE)
    # xarray stores dimension names in _ARRAY_DIMENSIONS
    ad <- attrs[["_ARRAY_DIMENSIONS"]]
    if (!is.null(ad)) {
      dim_names <- as.character(unlist(ad))
    }
  }

  # V2 chunk key separator is "." by default
  sep <- meta[["dimension_separator"]] %||% "."

  ZaroMeta(
    zarr_format = 2L,
    node_type = "array",
    shape = shape,
    data_type = dtype_parsed$dtype,
    chunk_shape = chunk_shape,
    codecs = codecs,
    fill_value = fill_value,
    dimension_names = dim_names,
    attributes = as.list(attrs),
    chunk_key_sep = sep,
    raw_meta = as.list(meta)
  )
}


#' Parse a V2 .zgroup document
#'
#' @param zgroup_bytes raw vector containing .zgroup content
#' @param zattrs_bytes raw vector containing .zattrs content (or NULL)
#' @returns ZaroMeta object
#' @noRd
parse_zgroup <- function(zgroup_bytes, zattrs_bytes = NULL) {
  meta <- jsonlite::fromJSON(sanitize_json(rawToChar(zgroup_bytes)), simplifyVector = FALSE)

  attrs <- list()
  if (!is.null(zattrs_bytes)) {
    attrs <- jsonlite::fromJSON(sanitize_json(rawToChar(zattrs_bytes)), simplifyVector = FALSE)
  }

  ZaroMeta(
    zarr_format = 2L,
    node_type = "group",
    shape = integer(0),
    data_type = "",
    chunk_shape = integer(0),
    codecs = list(),
    fill_value = NULL,
    dimension_names = NULL,
    attributes = as.list(attrs),
    chunk_key_sep = ".",
    raw_meta = as.list(meta)
  )
}


#' Parse a numpy dtype string to V3 data_type name and endianness
#'
#' Numpy dtype strings: "<f4" (little-endian float32), ">i2" (big-endian int16),
#' "|u1" (byte-order irrelevant uint8), etc.
#'
#' @param dtype_str character, e.g. "<f4", ">i2", "|b1"
#' @returns list with dtype (V3-style name) and endian ("little", "big", or "little")
#' @noRd
parse_numpy_dtype <- function(dtype_str) {
  # handle bare type codes (no endian prefix)
  if (nchar(dtype_str) < 2) {
    stop("cannot parse dtype: ", dtype_str, call. = FALSE)
  }

  # string types
  # <U59, |U100 = fixed-width Unicode (UTF-32 code units)
  # |S10, |S100 = fixed-width byte strings (NUL padded)
  # |O = object (variable-length strings in V2, with a vlen-utf8 filter)
  if (grepl("^[<>|=]?[USO]", dtype_str)) {
    code <- sub("^[<>|=]?", "", dtype_str)
    dtype <- switch(substr(code, 1, 1), U = "fixed_length_utf32",
                    S = "null_terminated_bytes", O = "string")
    return(list(dtype = dtype,
                endian = if (startsWith(dtype_str, ">")) "big" else "little"))
  }
  endian_char <- substr(dtype_str, 1, 1)
  type_code <- substr(dtype_str, 2, 2)
  type_size <- as.integer(substr(dtype_str, 3, nchar(dtype_str)))

  endian <- switch(endian_char,
    "<" = "little",
    ">" = "big",
    "|" = "little",  # byte-order not applicable (single byte types)
    "=" = .Platform$endian,  # native
    "little"  # default fallback
  )

  dtype <- switch(paste0(type_code, type_size),
    "f2" = "float16",
    "f4" = "float32",
    "f8" = "float64",
    "i1" = "int8",
    "i2" = "int16",
    "i4" = "int32",
    "i8" = "int64",
    "u1" = "uint8",
    "u2" = "uint16",
    "u4" = "uint32",
    "u8" = "uint64",
    "b1" = "bool",
    "S1" = "string",
    stop("unsupported numpy dtype: ", dtype_str, call. = FALSE)
  )

  list(dtype = dtype, endian = endian)
}


#' Map V2 compressor + filters to a V3-style codec list
#'
#' V2 has a single "compressor" dict (e.g. {"id": "zstd", "level": 3}) and
#' an optional "filters" list. We map these to the codec list format used by
#' ZaroMeta so the codec pipeline can handle V2 and V3 uniformly.
#'
#' @param compressor list or NULL (the V2 compressor dict)
#' @param filters list or NULL (the V2 filters list)
#' @param endian character, endianness from dtype parsing
#' @returns list of codec specs (name + configuration)
#' @noRd
v2_codecs <- function(compressor, filters, endian = "little") {
  codecs <- list()

  # bytes/endian codec (always present conceptually)
  codecs[[length(codecs) + 1L]] <- list(
    name = "bytes",
    configuration = list(endian = endian)
  )

  # filters come before compression in V2
  if (!is.null(filters)) {
    for (filt in filters) {
      if (is.null(filt)) next
      codecs[[length(codecs) + 1L]] <- list(
        name = filt[["id"]] %||% "unknown",
        configuration = filt
      )
    }
  }

  # compressor
  if (!is.null(compressor)) {
    codec_name <- compressor[["id"]]
    if (!is.null(codec_name)) {
      codecs[[length(codecs) + 1L]] <- list(
        name = codec_name,
        configuration = compressor
      )
    }
  }

  codecs
}

#' Parse V2 consolidated metadata (.zmetadata)
#'
#' Returns a named list of ZaroMeta objects keyed by variable path,
#' built from the .zarray and .zattrs entries in .zmetadata.
#'
#' @param raw_bytes raw vector containing .zmetadata content
#' @returns named list of ZaroMeta objects, or NULL if not valid
#' @noRd
parse_zmetadata <- function(raw_bytes) {

  txt <- rawToChar(raw_bytes)
  # Python writes bare NaN/Infinity which are not valid JSON
  txt <- sanitize_json(txt)


  doc <- jsonlite::fromJSON(txt, simplifyVector = FALSE)

  entries <- doc[["metadata"]]
  if (is.null(entries)) return(NULL)

  # collect .zarray keys and their paths
  zarray_keys <- grep("/\\.zarray$", names(entries), value = TRUE)

  result <- list()
  for (key in zarray_keys) {
    var_path <- sub("/\\.zarray$", "", key)

    # exact numbers, and JSON `null` (e.g. fill_value) stays null rather
    # than degrading to an empty object `{}`
    zarray_json <- to_json_exact(entries[[key]])

    # find matching .zattrs
    zattrs_key <- paste0(var_path, "/.zattrs")
    zattrs_json <- NULL
    if (zattrs_key %in% names(entries)) {
      zattrs_json <- charToRaw(to_json_exact(entries[[zattrs_key]]))
    }

    result[[var_path]] <- parse_zarray(charToRaw(zarray_json), zattrs_json)
  }

  # nested groups (the root's own .zgroup is read by zaro_meta())
  for (key in grep("/\\.zgroup$", names(entries), value = TRUE)) {
    grp_path <- sub("/\\.zgroup$", "", key)
    zattrs_key <- paste0(grp_path, "/.zattrs")
    zattrs_raw <- if (zattrs_key %in% names(entries)) {
      charToRaw(to_json_exact(entries[[zattrs_key]]))
    }
    result[[grp_path]] <- parse_zgroup(charToRaw(to_json_exact(entries[[key]])),
                                       zattrs_raw)
  }

  result
}


