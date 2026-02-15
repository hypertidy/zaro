# print.R — display methods

method(print, ZaroMeta) <- function(x, ...) {
  cat(sprintf("<ZaroMeta> [%s]\n", x@node_type))

  if (x@node_type == "array") {
    shape_str <- paste(x@shape, collapse = " x ")
    chunk_str <- paste(x@chunk_shape, collapse = " x ")
    cat(sprintf("  dtype:  %s\n", x@data_type))
    cat(sprintf("  shape:  [%s]\n", shape_str))
    cat(sprintf("  chunks: [%s]\n", chunk_str))

    if (!is.null(x@dimension_names)) {
      cat(sprintf("  dims:   %s\n", paste(x@dimension_names, collapse = ", ")))
    }

    n_codecs <- length(x@codecs)
    if (n_codecs > 0) {
      codec_names <- vapply(x@codecs, function(c) c$name, character(1))
      cat(sprintf("  codecs: %s\n", paste(codec_names, collapse = " -> ")))
    }

    if (!is.null(x@fill_value)) {
      cat(sprintf("  fill:   %s\n", as.character(x@fill_value)))
    }

    nbytes <- prod(as.double(x@shape)) * dtype_size(x@data_type)
    cat(sprintf("  size:   %s\n", format_bytes(nbytes)))
  }

  if (length(x@attributes) > 0) {
    cat(sprintf("  attrs:  %d\n", length(x@attributes)))
  }

  invisible(x)
}

method(print, ZaroStore) <- function(x, ...) {
  cls <- class(x)[1]
  cat(sprintf("<ZaroStore> [%s]\n", cls))
  cat(sprintf("  root: %s\n", x@root))
  invisible(x)
}


#' Format byte count for display
#' @noRd
format_bytes <- function(n) {
  units <- c("B", "KB", "MB", "GB", "TB")
  i <- 1L
  while (n >= 1024 && i < length(units)) {
    n <- n / 1024
    i <- i + 1L
  }
  sprintf("%.1f %s", n, units[i])
}
