# meta.R — Zarr V3 metadata parsing
#
# Zarr V3 metadata lives in zarr.json files at each node.
# Arrays have: shape, data_type, chunk_grid, chunk_key_encoding,
#              codecs, fill_value, dimension_names, attributes
# Groups have: node_type = "group", attributes, consolidated_metadata

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

  chunk_grid <- meta[["chunk_grid"]]
  chunk_shape <- as.integer(unlist(chunk_grid[["configuration"]][["chunk_shape"]]))

  chunk_key_enc <- meta[["chunk_key_encoding"]]
  chunk_key_sep <- chunk_key_enc[["configuration"]][["separator"]] %||% "/"

  codecs <- parse_codecs(meta[["codecs"]])

  dim_names <- meta[["dimension_names"]]
  if (!is.null(dim_names)) {
    dim_names <- as.character(unlist(dim_names))
    if (all(dim_names == "")) dim_names <- NULL
  }

  ZaroMeta(
    zarr_format = zarr_format,
    node_type = "array",
    shape = shape,
    data_type = meta[["data_type"]],
    chunk_shape = chunk_shape,
    codecs = codecs,
    fill_value = meta[["fill_value"]],
    dimension_names = dim_names,
    attributes = as.list(meta[["attributes"]] %||% list()),
    chunk_key_sep = chunk_key_sep,
    raw_meta = as.list(meta)
  )
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
#' @param meta ZaroMeta for the array
#' @param idx integer vector of chunk grid indices (0-based)
#' @returns character key suffix e.g. "c/0/1/2"
#' @noRd
chunk_key <- function(meta, idx) {
  paste0("c/", paste(idx, collapse = meta@chunk_key_sep))
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
  meta <- jsonlite::fromJSON(txt, simplifyVector = FALSE)

  cm <- meta[["consolidated_metadata"]]
  if (is.null(cm)) return(NULL)

  entries <- cm[["metadata"]]
  if (is.null(entries)) return(NULL)

  result <- list()
  for (path in names(entries)) {
    entry_json <- jsonlite::toJSON(entries[[path]], auto_unbox = TRUE)
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
    stop("unsupported dtype: ", dtype, call. = FALSE)
  )
}

#' Number of bytes per element for a dtype
#' @noRd
dtype_size <- function(dtype) {
  dtype_info(dtype)$size
}
