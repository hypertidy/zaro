# utils.R — small helpers

#' Null coalescing operator
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Emit a verbose message (suppressed by suppressMessages)
#' @noRd
vmsg <- function(..., verbose = TRUE) {
  if (isTRUE(verbose)) message("[zaro] ", ...)
}


#' Sanitize Python-flavoured JSON for R's jsonlite
#'
#' Replaces bare NaN, Infinity, -Infinity with quoted strings.
#' @noRd
sanitize_json <- function(txt) {
  txt <- gsub(":[ \t]*NaN\\b", ":\"NaN\"", txt)
  txt <- gsub(":[ \t]*Infinity\\b", ":\"Infinity\"", txt)
  txt <- gsub(":[ \t]*-Infinity\\b", ":\"-Infinity\"", txt)
  txt
}
## the bare NaN thing look out for
# store <- zaro("virtualizarr://https://raw.githubusercontent.com/mdsumner/virtualized/refs/heads/main/remote/ocean_temp_2023.parq")
# [zaro] opening VirtualiZarr Parquet reference store: https://raw.githubusercontent.com/mdsumner/virtualized/refs/heads/main/remote/ocean_temp_2023.parq> meta <- zaro_meta(store)
# [zaro] found .zmetadata (Zarr V2 consolidated)
#
# Error: lexical error: invalid char in json text.           ar":"GREGORIAN","_FillValue":NaN,"_ARRAY_DIMENSIONS":["Time"                      (right here) --
#
#
