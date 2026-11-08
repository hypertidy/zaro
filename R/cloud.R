# cloud.R -- S3 and GCS objects over HTTPS, so zaro_fetch() can pool them
#
# cloud_access()   how a store was opened: anonymous, region, endpoint
# cloud_https()    s3:// gs:// /vsis3/ /vsigs/ paths -> HTTPS URLs, plus the
#                  curl options that sign them (or NA: leave to Arrow/GDAL)
# s3_bucket_region()  a bucket's region, from S3's x-amz-bucket-region
#                  header, looked up once per bucket and cached
#
# Rewriting happens only when the access is known: anonymous (asked for, a
# known public bucket, AWS_NO_SIGN_REQUEST, or zaro.cloud_anonymous), or
# signed with AWS keys in the environment (curl's SigV4). Anything else
# (credential profiles, instance roles, signed GCS) stays with Arrow or
# GDAL, read one object at a time. Guessing "anonymous" is not safe: S3
# answers 403 for a missing key, which a whole-object read takes as an
# absent chunk, so a private bucket read anonymously would look all-fill.


#' The access a cloud store was opened with
#'
#' @param dots the `...` given to [zaro()]: `anonymous`, `region`,
#'   `endpoint_override` (Arrow's names) are kept.
#' @noRd
cloud_access <- function(dots) {
  keep <- intersect(names(dots), c("anonymous", "region", "endpoint_override",
                                   "scheme"))
  dots[keep]
}

.zaro_cloud <- new.env(parent = emptyenv())   # bucket -> region

#' A bucket's region, from the x-amz-bucket-region response header
#'
#' S3 sends the header on any response for the bucket, 403 and 301
#' included, so an anonymous HEAD answers it. Cached per bucket.
#' @noRd
s3_bucket_region <- function(bucket) {
  hit <- .zaro_cloud[[bucket]]
  if (!is.null(hit)) return(hit)
  h <- curl::new_handle(nobody = TRUE, timeout = 20)
  r <- tryCatch(curl::curl_fetch_memory(
    sprintf("https://%s.s3.amazonaws.com/", bucket), handle = h),
    error = function(e) NULL)
  region <- NULL
  if (!is.null(r)) {
    hdr <- curl::parse_headers_list(r$headers)
    region <- hdr[["x-amz-bucket-region"]]
  }
  if (is.null(region) || !nzchar(region)) {
    region <- Sys.getenv("AWS_REGION", Sys.getenv("AWS_DEFAULT_REGION", "us-east-1"))
  }
  assign(bucket, region, envir = .zaro_cloud)
  region
}

#' Escape each segment of an object key (not the slashes)
#' @noRd
escape_key <- function(key) {
  vapply(strsplit(key, "/", fixed = TRUE), function(p) {
    paste(curl::curl_escape(p), collapse = "/")
  }, "")
}

#' AWS keys from the environment, or NULL
#' @noRd
env_aws_keys <- function() {
  id <- Sys.getenv("AWS_ACCESS_KEY_ID")
  secret <- Sys.getenv("AWS_SECRET_ACCESS_KEY")
  if (!nzchar(id) || !nzchar(secret)) return(NULL)
  list(id = id, secret = secret, token = Sys.getenv("AWS_SESSION_TOKEN"))
}

#' Cloud paths as HTTPS URLs, with the curl options to sign them
#'
#' @param path character: `s3://bucket/key`, `gs://bucket/key`,
#'   `/vsis3/bucket/key` or `/vsigs/bucket/key`.
#' @param access a `cloud_access()` list (the store's); refs from a virtual
#'   store have none, and use the environment and options alone.
#' @returns list(url, opts): `url` NA where the path is left to Arrow/GDAL;
#'   `opts` a list of curl handle options (or NULL) per path.
#' @noRd
cloud_https <- function(path, access = list()) {
  n <- length(path)
  url <- rep(NA_character_, n)
  opts <- vector("list", n)
  p <- sub("^/vsis3/", "s3://", sub("^/vsigs/", "gs://", path))
  scheme <- substr(p, 1L, 2L)
  rest <- sub("^(s3|gs)://", "", p)
  bucket <- sub("/.*$", "", rest)
  key <- ifelse(grepl("/", rest, fixed = TRUE), sub("^[^/]*/", "", rest), "")
  ok <- grepl("^(s3|gs)://", p) & nzchar(bucket)

  explicit_anon <- isTRUE(access[["anonymous"]]) ||
    isTRUE(getOption("zaro.cloud_anonymous", FALSE))
  for (b in unique(bucket[ok])) {
    i <- which(ok & bucket == b)
    s <- scheme[i[1L]]
    anon <- explicit_anon || is_known_public(b) ||
      (s == "s3" && toupper(Sys.getenv("AWS_NO_SIGN_REQUEST")) == "YES")
    if (s == "gs") {
      if (!anon) next                               # signed GCS: Arrow/GDAL
      url[i] <- paste0("https://storage.googleapis.com/", b, "/",
                       escape_key(key[i]))
      next
    }
    keys <- if (anon) NULL else env_aws_keys()
    if (!anon && is.null(keys)) next                # profiles, roles: Arrow/GDAL
    endpoint <- access[["endpoint_override"]] %||% Sys.getenv("AWS_ENDPOINT_URL")
    region <- access[["region"]]
    if (nzchar(endpoint)) {
      if (!grepl("^https?://", endpoint)) {
        endpoint <- paste0(access[["scheme"]] %||% "https", "://", endpoint)
      }
      url[i] <- paste0(sub("/+$", "", endpoint), "/", b, "/", escape_key(key[i]))
      region <- region %||% Sys.getenv("AWS_REGION", "us-east-1")
    } else {
      region <- region %||% s3_bucket_region(b)
      url[i] <- if (grepl(".", b, fixed = TRUE)) {
        # dotted bucket names don't match the wildcard certificate
        paste0("https://s3.", region, ".amazonaws.com/", b, "/", escape_key(key[i]))
      } else {
        paste0("https://", b, ".s3.", region, ".amazonaws.com/", escape_key(key[i]))
      }
    }
    if (!is.null(keys)) {
      o <- list(aws_sigv4 = paste0("aws:amz:", region, ":s3"),
                userpwd = paste0(keys$id, ":", keys$secret))
      if (nzchar(keys$token)) o$headers <- c(`x-amz-security-token` = keys$token)
      opts[i] <- list(o)
    }
  }
  list(url = url, opts = opts)
}

#' Plan rows that can go through the curl pool as HTTPS
#'
#' Rows whose transport is s3, gs, or store/vsi with a cloud path (native
#' chunks of an Arrow or VSI store) are rewritten in place: transport
#' "http", path the HTTPS URL, and attr "curl_opts" holding the signing
#' options per row.
#' @noRd
plan_cloud_to_http <- function(plan, store) {
  cloudy <- which(plan$transport %in% c("s3", "gs", "store", "vsi") &
                    grepl("^(s3://|gs://|/vsis3/|/vsigs/)", plan$path))
  opts <- vector("list", nrow(plan))
  if (length(cloudy)) {
    access <- if (S7::S7_inherits(store, ZaroStore)) store@cloud else list()
    ch <- cloud_https(plan$path[cloudy], access)
    hit <- !is.na(ch$url)
    rows <- cloudy[hit]
    plan$path[rows] <- ch$url[hit]
    plan$transport[rows] <- "http"
    opts[rows] <- ch$opts[hit]
  }
  attr(plan, "curl_opts") <- opts
  plan
}
