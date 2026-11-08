# parquet-ranged.R -- read some row groups of a large Parquet file, by range
#
# nanoparquet reads whole files. A flat reference table can be hundreds of MB
# (BRAN2023 temp: 37.5M rows, 324 MB in 36 row groups of 1M rows) while a
# selection needs one or two row groups. So:
#
#   pq_footer()     fetch the footer only (the last 8 bytes give its length),
#                   and read its metadata (row groups, per-column min/max)
#   pq_read_row_groups()
#                   fetch the chosen row groups' bytes (one range each, all
#                   through the curl pool), write them at their own offsets
#                   into a sparse local file, append a footer rewritten to
#                   list only those row groups, and read that with nanoparquet
#
# The footer is Thrift compact protocol. The rewrite parses only the top
# level of FileMetaData: field 3 (num_rows) is re-encoded and field 4
# (row_groups) keeps the chosen RowGroup structs byte for byte. Their column
# chunks give absolute file offsets, which stay true because the sparse file
# keeps the original layout.


# -- Thrift compact protocol: just enough to walk and copy --------------------

#' Read an unsigned varint at `pos` (1-based); returns list(value, pos)
#' @noRd
tc_varint <- function(b, pos) {
  v <- 0; shift <- 0
  repeat {
    x <- as.integer(b[pos]); pos <- pos + 1L
    v <- v + bitwAnd(x, 0x7FL) * 2^shift
    if (x < 128L) break
    shift <- shift + 7
  }
  list(value = v, pos = pos)
}

#' Encode a non-negative number as an unsigned varint
#' @noRd
tc_encode_varint <- function(v) {
  out <- raw()
  repeat {
    x <- v %% 128; v <- v %/% 128
    if (v > 0) { out <- c(out, as.raw(x + 128)) } else { out <- c(out, as.raw(x)); break }
  }
  out
}

#' Zigzag-encode a signed integer (exact below 2^52)
#' @noRd
tc_zigzag <- function(v) if (v >= 0) 2 * v else -2 * v - 1

#' Skip one value of compact type `t` starting at `pos`; returns the new pos
#' @noRd
tc_skip <- function(b, pos, t) {
  switch(as.character(t),
    "1" = , "2" = pos,                         # bool in a field header
    "3" = pos + 1L,                            # i8
    "4" = , "5" = , "6" = tc_varint(b, pos)$pos,
    "7" = pos + 8L,                            # double
    "8" = { l <- tc_varint(b, pos); l$pos + l$value },
    "9" = , "10" = {                           # list, set
      h <- as.integer(b[pos]); pos <- pos + 1L
      n <- bitwShiftR(h, 4L); et <- bitwAnd(h, 0x0FL)
      if (n == 15L) { l <- tc_varint(b, pos); n <- l$value; pos <- l$pos }
      for (i in seq_len(n)) pos <- if (et %in% c(1L, 2L)) pos + 1L else tc_skip(b, pos, et)
      pos
    },
    "11" = {                                   # map
      l <- tc_varint(b, pos); n <- l$value; pos <- l$pos
      if (n > 0) {
        kv <- as.integer(b[pos]); pos <- pos + 1L
        kt <- bitwShiftR(kv, 4L); vt <- bitwAnd(kv, 0x0FL)
        for (i in seq_len(n)) {
          pos <- if (kt %in% c(1L, 2L)) pos + 1L else tc_skip(b, pos, kt)
          pos <- if (vt %in% c(1L, 2L)) pos + 1L else tc_skip(b, pos, vt)
        }
      }
      pos
    },
    "12" = tc_skip_struct(b, pos),
    stop("thrift: unknown compact type ", t, call. = FALSE))
}

#' Skip a struct (fields up to STOP); returns the pos after STOP
#' @noRd
tc_skip_struct <- function(b, pos) {
  repeat {
    h <- as.integer(b[pos]); pos <- pos + 1L
    if (h == 0L) return(pos)
    if (bitwShiftR(h, 4L) == 0L) pos <- tc_varint(b, pos)$pos   # long field id
    pos <- tc_skip(b, pos, bitwAnd(h, 0x0FL))
  }
}

#' A FileMetaData footer keeping only some row groups
#'
#' @param foot raw: the Thrift-encoded FileMetaData (without the trailing
#'   length and magic).
#' @param keep 0-based row group ids to keep, in file order (NULL: all).
#' @param num_rows the kept row groups' row count.
#' @noRd
pq_footer_subset <- function(foot, keep, num_rows) {
  out <- list(); pos <- 1L; fid <- 0L
  repeat {
    start <- pos
    h <- as.integer(foot[pos]); pos <- pos + 1L
    if (h == 0L) { out[[length(out) + 1L]] <- foot[start]; break }
    d <- bitwShiftR(h, 4L); t <- bitwAnd(h, 0x0FL)
    if (d == 0L) {
      l <- tc_varint(foot, pos); pos <- l$pos
      z <- l$value; fid <- if (z %% 2 == 0) z / 2 else -(z + 1) / 2
    } else fid <- fid + d
    hdr <- foot[start:(pos - 1L)]
    vstart <- pos
    if (fid == 3L && t == 6L) {                     # num_rows (i64)
      pos <- tc_skip(foot, pos, t)
      out[[length(out) + 1L]] <- c(hdr, tc_encode_varint(tc_zigzag(num_rows)))
    } else if (fid == 4L && t == 9L) {              # row_groups: list<RowGroup>
      lh <- as.integer(foot[pos]); pos <- pos + 1L
      n <- bitwShiftR(lh, 4L); et <- bitwAnd(lh, 0x0FL)
      if (n == 15L) { l <- tc_varint(foot, pos); n <- l$value; pos <- l$pos }
      spans <- vector("list", n)
      for (i in seq_len(n)) {
        s <- pos; pos <- tc_skip_struct(foot, pos); spans[[i]] <- foot[s:(pos - 1L)]
      }
      kept <- if (is.null(keep)) spans else spans[keep + 1L]
      k <- length(kept)
      lhdr <- if (k < 15L) as.raw(bitwOr(bitwShiftL(k, 4L), et)) else
        c(as.raw(bitwOr(0xF0L, et)), tc_encode_varint(k))
      out[[length(out) + 1L]] <- c(hdr, lhdr, unlist(kept))
    } else {
      pos <- tc_skip(foot, pos, t)
      out[[length(out) + 1L]] <- c(hdr, if (pos > vstart) foot[vstart:(pos - 1L)])
    }
  }
  unlist(out)
}


# -- byte ranges of a Parquet file -------------------------------------------

#' Bytes [offset, offset + length) of a file, local or remote, many at once
#'
#' http(s) and S3/GCS (as HTTPS when access is known) go through the curl
#' pool; other paths through byte_range_read().
#' @noRd
pq_ranges <- function(src, offset, length, store = NULL) {
  path <- src
  if (grepl("^(s3|gs)://", src)) {
    ch <- cloud_https(src, if (!is.null(store)) store@cloud else list())
    if (!is.na(ch$url)) path <- ch$url
    opts <- ch$opts
  } else opts <- NULL
  if (grepl("^https?://", path)) {
    return(http_fetch(rep(path, length(offset)), offset, length,
                      max_con = getOption("zaro.max_con", 8L),
                      opts = if (!is.null(opts)) rep(opts, length(offset))))
  }
  Map(function(o, l) byte_range_read(src, o, l), offset, length)
}

#' The last `n` bytes of a file, and its size, in one request
#'
#' A suffix range (`Range: bytes=-n`) answers both: the tail, and the size
#' in `Content-Range`. A server that ignores the range sends the whole file
#' (200), which answers both too.
#' @noRd
pq_tail <- function(src, n, store = NULL) {
  if (!grepl("^(https?|s3|gs)://", src)) {
    size <- file.size(src)
    if (is.na(size)) stop("no such file: ", src, call. = FALSE)
    n <- min(n, size)
    return(list(size = size, tail = byte_range_read(src, size - n, n)))
  }
  url <- src; opts <- NULL
  if (grepl("^(s3|gs)://", src)) {
    ch <- cloud_https(src, if (!is.null(store)) store@cloud else list())
    if (is.na(ch$url)) {                      # no known access: GDAL/Arrow
      vs <- cloud_to_vsi(src)
      size <- if (requireNamespace("gdalraster", quietly = TRUE))
        gdalraster::vsi_stat(vs, "size") else NA
      if (is.na(size)) stop("can't read ", src, call. = FALSE)
      n <- min(n, size)
      return(list(size = size, tail = byte_range_read(src, size - n, n)))
    }
    url <- ch$url; opts <- ch$opts[[1L]]
  }
  h <- curl::new_handle()
  hdrs <- c(opts$headers, Range = sprintf("bytes=-%.0f", n))
  opts$headers <- NULL
  if (length(opts)) curl::handle_setopt(h, .list = opts)
  curl::handle_setheaders(h, .list = as.list(hdrs))
  r <- curl::curl_fetch_memory(url, handle = h)
  if (r$status_code == 200L) {
    size <- length(r$content)
    return(list(size = size, tail = r$content[seq.int(max(1, size - n + 1), size)]))
  }
  if (r$status_code != 206L) stop("HTTP ", r$status_code, " for ", url, call. = FALSE)
  cr <- curl::parse_headers_list(r$headers)[["content-range"]]
  size <- suppressWarnings(as.numeric(sub("^.*/", "", cr %||% "")))
  if (is.na(size)) stop("no size in Content-Range for ", url, call. = FALSE)
  list(size = size, tail = r$content)
}


#' A large Parquet file's footer and metadata, without the rest
#'
#' @returns list(src, size, foot (raw Thrift), meta (nanoparquet metadata),
#'   rg (data frame: row group id, offset, length, num_rows))
#' @noRd
pq_footer <- function(src, store = NULL) {
  # the last 64 KB usually hold the whole footer: one request for the
  # footer and the file size
  tl <- pq_tail(src, 65536, store)
  size <- tl$size
  tail <- tl$tail
  n <- length(tail)
  t8 <- tail[(n - 7):n]
  if (!identical(rawToChar(t8[5:8]), "PAR1")) {
    stop("not a Parquet file: ", src, call. = FALSE)
  }
  mlen <- readBin(t8[1:4], "integer", size = 4L, endian = "little")
  foot <- if (mlen + 8 <= n) tail[(n - 7 - mlen):(n - 8)] else
    pq_ranges(src, size - 8 - mlen, mlen, store)[[1L]]
  # the metadata alone needs no data at its offsets, so a tiny file; and
  # with the file's num_rows set to 0 (each row group keeps its own), as
  # nanoparquet 0.5.1 allocates in proportion to num_rows when reading
  # metadata (2.7 GB for 37.5M rows)
  f0 <- pq_footer_subset(foot, NULL, 0)
  tf <- pq_sparse(4 + length(f0) + 8, list(), list(), f0)
  on.exit(unlink(tf), add = TRUE)
  meta <- nanoparquet::read_parquet_metadata(tf)
  cc <- meta$column_chunks
  start <- ifelse(is.na(cc$dictionary_page_offset) | cc$dictionary_page_offset <= 0,
                  cc$data_page_offset, pmin(cc$dictionary_page_offset, cc$data_page_offset))
  rg <- do.call(rbind, lapply(split(seq_len(nrow(cc)), cc$row_group), function(i) {
    data.frame(id = cc$row_group[i[1L]], offset = min(start[i]),
               length = max(start[i] + cc$total_compressed_size[i]) - min(start[i]))
  }))
  rg$num_rows <- meta$row_groups$num_rows[match(rg$id, meta$row_groups$id)]
  list(src = src, size = size, foot = foot, meta = meta, rg = rg[order(rg$id), ])
}

#' A sparse local file: PAR1, byte ranges at their offsets, a footer at the end
#' @noRd
pq_sparse <- function(size, offsets, chunks, foot) {
  tf <- tempfile(fileext = ".parquet")
  con <- file(tf, "wb")
  on.exit(close(con))
  writeBin(charToRaw("PAR1"), con)
  for (i in seq_along(chunks)) {
    seek(con, offsets[[i]], rw = "write")
    writeBin(chunks[[i]], con)
  }
  seek(con, size - 8 - length(foot), rw = "write")
  writeBin(c(foot, writeBin(length(foot), raw(), size = 4L, endian = "little"),
             charToRaw("PAR1")), con)
  tf
}

#' Min and max of an integer column per row group, from the statistics
#'
#' @returns data frame (id, min, max), NA where the file has no statistics.
#' @noRd
pq_int_stats <- function(pf, column) {
  cc <- pf$meta$column_chunks
  col <- which(pf$meta$schema$name == column) - 2L      # schema row 1 is the root
  if (!length(col)) stop("no column '", column, "'", call. = FALSE)
  s <- cc[cc$column == col, ]
  dec <- function(r) {
    if (is.null(r) || !length(r)) return(NA_real_)
    if (length(r) == 8L) {
      w <- readBin(r, "integer", n = 2L, size = 4L, endian = "little")
      lo <- if (w[1] < 0) w[1] + 2^32 else w[1]
      return(lo + w[2] * 2^32)
    }
    if (length(r) == 4L) return(readBin(r, "integer", size = 4L, endian = "little"))
    NA_real_
  }
  data.frame(id = s$row_group, min = vapply(s$min_value, dec, 1),
             max = vapply(s$max_value, dec, 1))
}

#' Fetch some row groups' bytes (one range each, through the curl pool)
#'
#' @returns a list of raw, one per id (sorted, unique).
#' @noRd
pq_fetch_row_groups <- function(pf, ids, store = NULL) {
  rg <- pf$rg[match(ids, pf$rg$id), ]
  pq_ranges(pf$src, rg$offset, rg$length, store)
}

#' Decode row groups from their fetched bytes
#'
#' @param chunks list of raw from pq_fetch_row_groups(), one per id.
#' @returns a data frame of those row groups' rows (blob columns as lists
#'   of raw), with attr "bytes" (bytes fetched).
#' @noRd
pq_decode_row_groups <- function(pf, ids, chunks, col_select = NULL) {
  rg <- pf$rg[match(ids, pf$rg$id), ]
  foot <- pq_footer_subset(pf$foot, ids, sum(rg$num_rows))
  tf <- pq_sparse(pf$size, as.list(rg$offset), chunks, foot)
  on.exit(unlink(tf), add = TRUE)
  tbl <- nanoparquet::read_parquet(tf, col_select = col_select)
  for (nm in names(tbl)) if (inherits(tbl[[nm]], "blob")) tbl[[nm]] <- unclass(tbl[[nm]])
  out <- as.data.frame(tbl, stringsAsFactors = FALSE)
  attr(out, "bytes") <- sum(rg$length)
  out
}

#' Read some row groups of a Parquet file
#'
#' @param pf a pq_footer().
#' @param ids 0-based row group ids.
#' @param col_select columns, as for nanoparquet::read_parquet().
#' @returns a data frame of those row groups' rows, attr "bytes".
#' @noRd
pq_read_row_groups <- function(pf, ids, col_select = NULL, store = NULL) {
  ids <- sort(unique(ids))
  pq_decode_row_groups(pf, ids, pq_fetch_row_groups(pf, ids, store), col_select)
}
