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


#' Extract bucket name from a cloud URI
#'
#' @returns bucket name or NULL if not a cloud URI
#' @noRd
extract_bucket <- function(source) {
  if (!grepl("^(s3|gs)://", source)) return(NULL)
  parts <- strsplit(sub("^(s3|gs)://", "", source), "/")[[1]]
  if (length(parts) >= 1) parts[1] else NULL
}


#' Check if a bucket is known to be public (anonymous access)
#' @noRd
is_known_public <- function(bucket) {
  known <- c(
    # GCS
    "cmip6",
    # S3 — AODN
    "aodn-cloud-optimised",
    # S3 — NOAA / NASA
    "mur-sst", "era5-pds", "noaa-goes16", "noaa-goes17",
    "noaa-gfs-bdp-pds", "noaa-ghcn-pds", "noaa-cdr-sea-ice-conc-monthly",
    "noaa-nexrad-level2",
    # S3 — Pangeo / Earthmover
    "pangeo-forge"
  )
  tolower(bucket) %in% known
}

