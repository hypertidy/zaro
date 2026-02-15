# utils.R — small helpers

#' Null coalescing operator
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Emit a verbose message (suppressed by suppressMessages)
#' @noRd
vmsg <- function(..., verbose = TRUE) {
  if (isTRUE(verbose)) message("[zaro] ", ...)
}
