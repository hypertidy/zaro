# codec.R - codec pipeline (V2 and V3)
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
  n <- prod(meta@chunk_shape)

  # determine endianness from the "bytes" codec if present
  endian <- chunk_endian(meta@codecs)

  read_typed(buf, meta@data_type, n, endian)
}


#' Reinterpret bytes as an R vector of a Zarr data type
#'
#' readBin() covers the types R holds natively. The rest are built here:
#' uint32 and 64-bit integers become double (exact up to 2^53), float16 is
#' decoded from its bit fields. `n` caps the number of elements read.
#' @noRd
read_typed <- function(buf, data_type, n, endian = "little") {
  info <- dtype_info(data_type)
  n <- min(n, length(buf) %/% info$size)
  switch(data_type,
    "uint32"  = {
      v <- readBin(buf, "integer", n = n, size = 4L, endian = endian)
      v <- as.double(v)
      v[!is.na(v) & v < 0] <- v[!is.na(v) & v < 0] + 2^32
      v[is.na(v)] <- 2^31   # readBin's NA is the bit pattern 0x80000000
      v
    },
    "int64"   = read_int64(buf, n, endian, signed = TRUE),
    "uint64"  = read_int64(buf, n, endian, signed = FALSE),
    "float16" = read_float16(buf, n, endian),
    readBin(buf, what = info$what, n = n, size = info$size,
            signed = info$signed, endian = endian)
  )
}

#' 64-bit integers as double, from 32-bit halves
#' @noRd
read_int64 <- function(buf, n, endian, signed) {
  w <- readBin(buf, "integer", n = 2L * n, size = 4L, endian = endian)
  w <- as.double(w)
  w[is.na(w)] <- -2^31                       # 0x80000000
  u <- ifelse(w < 0, w + 2^32, w)            # each half as unsigned
  idx <- seq_len(n)
  first <- u[2L * idx - 1L]
  second <- u[2L * idx]
  lo <- if (endian == "little") first else second
  hi <- if (endian == "little") second else first
  if (signed) hi <- ifelse(hi >= 2^31, hi - 2^32, hi)
  hi * 2^32 + lo
}

#' IEEE 754 half precision as double
#' @noRd
read_float16 <- function(buf, n, endian) {
  h <- readBin(buf, "integer", n = n, size = 2L, signed = FALSE,
               endian = endian)
  sgn <- ifelse(bitwAnd(h, 0x8000L) > 0L, -1, 1)
  e <- bitwShiftR(bitwAnd(h, 0x7C00L), 10L)
  m <- bitwAnd(h, 0x03FFL)
  v <- ifelse(e == 0L, m * 2^-24,                       # subnormal / zero
       ifelse(e == 31L, ifelse(m == 0L, Inf, NaN),      # inf / nan
              (1 + m / 1024) * 2^(e - 15)))
  sgn * v
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
  raw_buf <- arrow::buffer(buf)
  reader <- arrow::BufferReader$create(raw_buf)
  stream <- arrow::CompressedInputStream$create(reader, codec)

  # overestimate is fine - Read() returns only what's available
  out_size <- config[["decompressed_size"]]
  if (is.null(out_size)) {
    out_size <- length(buf) * 20L
  }

  as.raw(stream$Read(out_size))
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
