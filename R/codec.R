# codec.R — codec pipeline (V2 and V3)
#
# V3 codecs are a chain: array-to-array, array-to-bytes, bytes-to-bytes.
# V2 has a single compressor + optional filters, normalised to V3-style
# codec lists by v2_codecs() in meta.R.
# We walk the chain in reverse to decode a stored chunk.
#
# Arrow provides: zstd, gzip, lz4, brotli, snappy, bz2
# Blosc requires the blosc package (optional)
# The "bytes" codec handles endianness (serialization)
# The "transpose" codec reorders array dimensions

#' Decode a raw chunk through the codec pipeline
#'
#' @param raw_bytes raw vector of stored chunk data
#' @param meta ZaroMeta for the array
#' @returns numeric or integer vector (flat, length = prod(chunk_shape))
#' @noRd
decode_chunk <- function(raw_bytes, meta) {
  buf <- raw_bytes

  # walk codecs in reverse order
  for (codec in rev(meta@codecs)) {
    buf <- decode_one(buf, codec, meta)
  }

  # reinterpret bytes as typed R vector
  info <- dtype_info(meta@data_type)
  n <- prod(meta@chunk_shape)

  # determine endianness from the "bytes" codec if present
  endian <- chunk_endian(meta@codecs)

  readBin(buf, what = info$what, n = n, size = info$size,
          signed = info$signed, endian = endian)
}


#' Apply a single codec in decode (reverse) direction
#' @noRd
decode_one <- function(buf, codec, meta) {
  name <- codec$name
  config <- codec$configuration

  switch(name,
         # -- bytes-to-bytes codecs (compression) --
         "zstd"   = arrow_decompress(buf, "zstd", config),
         "gzip"   = arrow_decompress(buf, "gzip", config),
         "lz4"    = arrow_decompress(buf, "lz4", config),
         "brotli" = arrow_decompress(buf, "brotli", config),
         "snappy" = arrow_decompress(buf, "snappy", config),
         "bz2"    = arrow_decompress(buf, "bz2", config),

         # V2 zlib -- R's memDecompress handles raw zlib/deflate
         "zlib"   = memDecompress(buf, type = "gzip"),

         "blosc"  = blosc_decompress(buf),

         # V2 shuffle filter -- unshuffle bytes by element size
         "shuffle" = unshuffle(buf, config$elementsize %||% 1L),

         # -- array-to-bytes codecs --
         "bytes"  = buf,  # endian handling done at readBin stage
         "endian" = buf,  # alias

         # -- array-to-array codecs --
         "transpose" = buf,  # handled post-readBin via dim permutation

         # -- passthrough for unknown --
         {
           warning("unknown codec '", name, "', passing through unchanged",
                   call. = FALSE)
           buf
         }
  )
}


#' Decompress using arrow::Codec
#' @noRd
arrow_decompress <- function(buf, type, config) {
  codec <- arrow::Codec$create(type)

  # arrow decompression needs to know output size for some codecs
  # try uncompressed_size from config first, otherwise estimate
  out_size <- config[["decompressed_size"]]
  if (is.null(out_size)) {
    # conservative estimate; will be resized if needed
    out_size <- length(buf) * 20L
  }

  tryCatch(
    codec$Decompress(length(buf), buf, out_size),
    error = function(e) {
      # retry with larger buffer
      codec$Decompress(length(buf), buf, out_size * 10L)
    }
  )
}


#' Decompress using the blosc R package
#' @noRd
blosc_decompress <- function(buf) {
  if (!requireNamespace("blosc", quietly = TRUE)) {
    stop("blosc package required for blosc-compressed chunks\n",
         "install with: install.packages('blosc')", call. = FALSE)
  }
  blosc::blosc_decompress(buf)
}


#' Unshuffle bytes (reverse of the NumCodecs shuffle filter)
#'
#' The shuffle filter interleaves bytes by element stride for better
#' compression. For elementsize=2 and N elements, the shuffled layout
#' stores all high bytes first, then all low bytes. Unshuffle recombines
#' them into the original byte order.
#'
#' @param buf raw vector of shuffled bytes
#' @param elementsize integer, bytes per element (e.g. 2 for int16, 4 for float32)
#' @returns raw vector of unshuffled bytes
#' @noRd
unshuffle <- function(buf, elementsize) {
  n <- length(buf)
  if (elementsize <= 1L || n == 0L) return(buf)

  n_elements <- n %/% elementsize
  out <- raw(n)

  for (i in seq_len(elementsize)) {
    # bytes for this position within each element
    src_start <- (i - 1L) * n_elements + 1L
    src_end <- i * n_elements
    src_bytes <- buf[src_start:src_end]
    # distribute to every elementsize-th position
    out[seq.int(i, n, by = elementsize)] <- src_bytes
  }

  out
}


#' Extract endianness from the codec chain
#'
#' Looks for a "bytes" codec with endian configuration.
#' Defaults to little-endian (Zarr V3 default).
#' @noRd
chunk_endian <- function(codecs) {
  for (codec in codecs) {
    if (codec$name %in% c("bytes", "endian")) {
      e <- codec$configuration[["endian"]]
      if (!is.null(e)) {
        return(switch(e, "little" = "little", "big" = "big", "little"))
      }
    }
  }
  "little"
}
