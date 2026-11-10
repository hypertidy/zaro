# codec.R - codec pipeline (V2 and V3)
#
# V3 codecs are a chain: array-to-array, array-to-bytes, bytes-to-bytes.
# V2 has a single compressor + optional filters, normalised to V3-style
# codec lists by v2_codecs() in meta.R.
# We walk the chain in reverse to decode a stored chunk.
#
# base R decodes zstd, gzip, bz2 (memDecompress); arrow (Suggests) lz4,
# brotli, snappy
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

  if (is_string_dtype(meta@data_type)) return(decode_strings(buf, meta, n))

  # determine endianness from the "bytes" codec if present
  endian <- chunk_endian(meta@codecs)

  read_typed(buf, meta@data_type, n, endian)
}


#' Decode a chunk of strings to a character vector
#'
#' - fixed_length_utf32 (numpy `U`): code points, NUL padded
#' - null_terminated_bytes (numpy `S`): bytes, NUL padded
#' - string (vlen-utf8): a uint32 count, then per item a uint32 byte length
#'   and the UTF-8 bytes
#' @noRd
decode_strings <- function(buf, meta, n) {
  sp <- string_spec(meta)
  if (sp$kind == "vlen") {
    if (!any(vapply(meta@codecs, function(c) identical(c$name, "vlen-utf8"),
                    logical(1)))) {
      stop("variable-length strings need the vlen-utf8 codec (codecs: ",
           paste(vapply(meta@codecs, function(c) c$name %||% "?", ""),
                 collapse = ", "), ")", call. = FALSE)
    }
    return(decode_vlen_utf8(buf, n))
  }
  lb <- sp$length_bytes
  m <- min(n, length(buf) %/% max(lb, 1L))
  if (lb == 0L) return(rep("", n))
  if (sp$kind == "utf32") {
    cp <- readBin(buf, "integer", n = m * lb %/% 4L, size = 4L,
                  endian = sp$endian)
    cp <- matrix(cp, nrow = lb %/% 4L)
    out <- apply(cp, 2L, function(x) intToUtf8(x[x != 0L]))
  } else {
    b <- matrix(buf[seq_len(m * lb)], nrow = lb)
    out <- apply(b, 2L, function(x) rawToChar(x[x != as.raw(0L)]))
  }
  out <- as.character(out)
  Encoding(out) <- if (sp$kind == "utf32") "UTF-8" else "unknown"
  out
}

#' numcodecs / Zarr V3 vlen-utf8 encoding to a character vector
#' @noRd
decode_vlen_utf8 <- function(buf, n) {
  count <- readBin(buf[1:4], "integer", size = 4L, endian = "little")
  out <- character(count)
  pos <- 5L
  for (i in seq_len(count)) {
    len <- readBin(buf[pos:(pos + 3L)], "integer", size = 4L,
                   endian = "little")
    pos <- pos + 4L
    out[i] <- if (len > 0L) rawToChar(buf[pos:(pos + len - 1L)]) else ""
    pos <- pos + len
  }
  Encoding(out) <- "UTF-8"
  out
}


#' decode_chunk() with the chunk's plan row named in any error
#'
#' A virtual reference whose source file was rewritten after the references
#' were made fetches the wrong bytes, which then fail to decompress with a
#' bare codec error. Say which chunk and byte range, and that the
#' references may be stale.
#' @noRd
decode_planned <- function(raw, meta, ref = NULL) {
  if (is.null(ref)) return(decode_chunk(raw, meta))
  tryCatch(decode_chunk(raw, meta), error = function(e) {
    where <- if (!is.na(ref$size)) {
      sprintf("%s [offset %.0f, %.0f bytes]", ref$path, ref$offset, ref$size)
    } else ref$path
    stop("decoding chunk '", ref$key, "' from ", where, " failed: ",
         conditionMessage(e),
         if (!is.na(ref$size)) paste0(
           "\n  (for a virtual store this often means the source file changed ",
           "after the references were made)"), call. = FALSE)
  })
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
  # Zarr V3 names for numcodecs codecs (written by zarr-python and
  # VirtualiZarr for netCDF-4/HDF5 sources, e.g. shuffle + zlib)
  if (startsWith(name, "numcodecs.")) {
    base <- substring(name, 11L)
    if (base %in% c("zlib", "gzip", "zstd", "bz2", "lz4", "blosc",
                    "shuffle", "crc32c")) name <- base
  }

  switch(name,
         # -- bytes-to-bytes codecs (compression) --
         # base R first (memDecompress), arrow when base can't
         "zstd"   = base_decompress(buf, "zstd", config),
         "gzip"   = base_decompress(buf, "gzip", config),
         "bz2"    = base_decompress(buf, "bz2", config),
         # arrow only
         "lz4"    = arrow_decompress(buf, "lz4", config),
         "brotli" = arrow_decompress(buf, "brotli", config),
         "snappy" = arrow_decompress(buf, "snappy", config),

         # V2 zlib -- R's memDecompress handles raw zlib/deflate
         "zlib"   = memDecompress(buf, type = "gzip"),

         "blosc"  = blosc_decompress(buf),

         # V3 crc32c: a 4-byte checksum appended; stripped, not verified
         "crc32c" = buf[seq_len(max(0L, length(buf) - 4L))],

         # V2 shuffle filter -- unshuffle bytes by element size
         # (V3 numcodecs.shuffle may leave elementsize to the data type)
         "shuffle" = unshuffle(buf, config$elementsize %||%
                                 dtype_info(meta@data_type)$size %||% 1L),

         # TIFF predictor 2: horizontal differencing (virtualized COG tiles)
         "imagecodecs_delta" = ,
         "horizontal_delta" = undo_horizontal_delta(buf, config, meta),

         # -- array-to-bytes codecs --
         "bytes"  = buf,  # endian handling done at readBin stage
         "endian" = buf,  # alias
         # variable-length strings: parsed in decode_strings()
         "vlen-utf8" = buf,

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


#' Undo horizontal differencing (TIFF predictor 2, imagecodecs `delta`)
#'
#' Each row along the last axis was stored as differences from the previous
#' element, in the sample's integer type with wraparound. Decoding is a
#' cumulative sum along that axis, wrapped back into the type's range. The
#' filter's own `dtype` and `shape` are used when given (imagecodecs writes
#' them), else the array's. Integer types up to 32 bits; `axis = -1`,
#' `dist = 1` only (what TIFF predictor 2 is).
#' @noRd
undo_horizontal_delta <- function(buf, config, meta) {
  axis <- as.integer(config[["axis"]] %||% -1L)
  dist <- as.integer(config[["dist"]] %||% 1L)
  shape <- as.integer(unlist(config[["shape"]] %||% meta@chunk_shape))
  if (!(axis == -1L || axis == length(shape) - 1L) || dist != 1L) {
    stop("horizontal delta: only axis = -1 and dist = 1 are supported (got axis ",
         axis, ", dist ", dist, ")", call. = FALSE)
  }
  dt <- if (!is.null(config[["dtype"]])) parse_numpy_dtype(config[["dtype"]]) else
    list(dtype = meta@data_type, endian = chunk_endian(meta@codecs))
  bits <- switch(dt$dtype, int8 = 8L, uint8 = 8L, int16 = 16L, uint16 = 16L,
                 int32 = 32L, uint32 = 32L,
                 stop("horizontal delta: unsupported dtype '", dt$dtype,
                      "' (integers up to 32 bits only; TIFF predictor 3 is not",
                      " supported)", call. = FALSE))
  signed <- startsWith(dt$dtype, "int")
  n <- prod(shape)
  v <- as.double(read_typed(buf, dt$dtype, n, dt$endian))
  ncol <- shape[length(shape)]
  m <- matrix(v, ncol = ncol, byrow = TRUE)          # one row per line
  if (ncol > 1L) for (k in 2:ncol) m[, k] <- m[, k] + m[, k - 1L]
  # wrap into the type's range (exact in double: |sum| < 2^53)
  m <- if (signed) (m + 2^(bits - 1)) %% 2^bits - 2^(bits - 1) else m %% 2^bits
  v <- as.vector(t(m))
  # write back as the same bytes: signed representation for writeBin
  if (!signed) v <- ifelse(v >= 2^(bits - 1), v - 2^bits, v)
  w <- rep(NA_integer_, length(v))                   # NA_integer_ is 0x80000000
  ok <- v != -2^31
  w[ok] <- as.integer(v[ok])
  writeBin(w, raw(), size = bits %/% 8L, endian = dt$endian)
}


#' Decompress with base R's memDecompress(), falling back to arrow
#'
#' gzip and bzip2 are always in base R; zstd is when R was built with
#' libzstd (R >= 4.5). A failure (codec unavailable, or a stream base R
#' can't read) falls back to arrow when it is installed.
#' @noRd
base_decompress <- function(buf, type, config) {
  base_type <- switch(type, bz2 = "bzip2", type)
  out <- tryCatch(memDecompress(buf, type = base_type), error = function(e) e)
  if (!inherits(out, "error")) return(out)
  if (has_arrow()) return(arrow_decompress(buf, type, config))
  stop("can't decompress ", type, " chunk with base R (",
       conditionMessage(out), "); installing 'arrow' adds another decoder",
       call. = FALSE)
}

#' Decompress using arrow::Codec
#' @noRd
arrow_decompress <- function(buf, type, config) {
  need_arrow(paste0("the ", type, " codec"))
  codec <- arrow::Codec$create(type)
  raw_buf <- arrow::buffer(buf)
  reader <- arrow::BufferReader$create(raw_buf)
  stream <- arrow::CompressedInputStream$create(reader, codec)

  # read until the stream is exhausted: a fixed guess at the output size
  # (it was 20x the input) truncates chunks that compress better than that,
  # such as constant or all-fill chunks
  step <- max(length(buf) * 20, 1024^2)
  parts <- list()
  repeat {
    b <- as.raw(stream$Read(step))
    if (!length(b)) break
    parts[[length(parts) + 1L]] <- b
  }
  if (!length(parts)) raw() else do.call(c, parts)
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
