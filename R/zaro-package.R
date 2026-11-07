#' @keywords internal
"_PACKAGE"

#' @import S7
NULL

.onLoad <- function(libname, pkgname) {
  # S7 methods on base generics (print) are registered here
  S7::methods_register()
}
