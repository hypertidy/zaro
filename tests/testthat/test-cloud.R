## S3 and GCS objects through the curl pool as HTTPS (cloud.R)

## env vars set for one test, restored when it ends (as withr does)
local_env <- function(..., .envir = parent.frame()) {
  vars <- c(...)
  old <- Sys.getenv(names(vars), unset = NA, names = TRUE)
  do.call(Sys.setenv, as.list(vars))
  reset <- function() {
    for (nm in names(old)) {
      if (is.na(old[[nm]])) Sys.unsetenv(nm) else
        do.call(Sys.setenv, stats::setNames(list(old[[nm]]), nm))
    }
  }
  do.call(on.exit, list(bquote(.(reset)()), add = TRUE), envir = .envir)
  invisible(old)
}
no_aws_env <- c(AWS_ACCESS_KEY_ID = "", AWS_SECRET_ACCESS_KEY = "",
                AWS_SESSION_TOKEN = "", AWS_NO_SIGN_REQUEST = "",
                AWS_ENDPOINT_URL = "")

test_that("anonymous S3: virtual-hosted HTTPS in the bucket's region", {
  local_env(no_aws_env)
  ch <- cloud_https(c("s3://my-bucket/a/b/0.0", "/vsis3/my-bucket/a b/1"),
                    list(anonymous = TRUE, region = "ap-southeast-2"))
  expect_identical(ch$url, c(
    "https://my-bucket.s3.ap-southeast-2.amazonaws.com/a/b/0.0",
    "https://my-bucket.s3.ap-southeast-2.amazonaws.com/a%20b/1"))
  expect_null(ch$opts[[1]])

  ## the region comes from the per-bucket cache when not given
  assign("cached-bucket", "eu-west-1", envir = .zaro_cloud)
  ch <- cloud_https("s3://cached-bucket/k", list(anonymous = TRUE))
  expect_identical(ch$url, "https://cached-bucket.s3.eu-west-1.amazonaws.com/k")

  ## dotted names: path-style
  ch <- cloud_https("s3://a.b.c/k", list(anonymous = TRUE, region = "us-west-2"))
  expect_identical(ch$url, "https://s3.us-west-2.amazonaws.com/a.b.c/k")
})

test_that("an endpoint override gives path-style URLs", {
  local_env(no_aws_env)
  ch <- cloud_https("s3://bkt/x/y", list(anonymous = TRUE,
                                         endpoint_override = "s3.example.org"))
  expect_identical(ch$url, "https://s3.example.org/bkt/x/y")
  local_env(AWS_ENDPOINT_URL = "http://localhost:9000/")
  ch <- cloud_https("s3://bkt/x", list(anonymous = TRUE))
  expect_identical(ch$url, "http://localhost:9000/bkt/x")
})

test_that("unknown access is left to Arrow/GDAL; env keys are signed", {
  local_env(no_aws_env)
  expect_true(is.na(cloud_https("s3://private-bucket/k", list())$url))
  expect_true(is.na(cloud_https("gs://private-bucket/k", list())$url))

  local_env(AWS_ACCESS_KEY_ID = "AKID", AWS_SECRET_ACCESS_KEY = "SECRET",
            AWS_SESSION_TOKEN = "TOKEN")
  ch <- cloud_https("s3://private-bucket/k", list(region = "us-east-1"))
  expect_identical(ch$url, "https://private-bucket.s3.us-east-1.amazonaws.com/k")
  expect_identical(ch$opts[[1]]$aws_sigv4, "aws:amz:us-east-1:s3")
  expect_identical(ch$opts[[1]]$userpwd, "AKID:SECRET")
  expect_identical(ch$opts[[1]]$headers[["x-amz-security-token"]], "TOKEN")
})

test_that("AWS_NO_SIGN_REQUEST and zaro.cloud_anonymous mean anonymous", {
  local_env(no_aws_env)
  local_env(AWS_NO_SIGN_REQUEST = "YES")
  ch <- cloud_https("s3://b2/k", list(region = "us-east-1"))
  expect_false(is.na(ch$url))
  local_env(AWS_NO_SIGN_REQUEST = "")
  old <- options(zaro.cloud_anonymous = TRUE)
  on.exit(options(old), add = TRUE)
  expect_identical(cloud_https("gs://b3/k/0", list())$url,
                   "https://storage.googleapis.com/b3/k/0")
})

test_that("plan rows: cloud paths become http, others untouched", {
  local_env(no_aws_env)
  plan <- refs_frame(c("a", "b", "c", "d"),
                     path = c("s3://pub/a", "/tmp/b", "https://h/c", "s3://priv/d"),
                     offset = c(0, 0, 0, 0),
                     transport = c("s3", "local", "http", "s3"))
  st <- LocalStore(root = tempdir())
  st@cloud <- list(region = "us-east-1")
  assign("pub", "us-east-1", envir = .zaro_cloud)
  old <- options(zaro.cloud_anonymous = NULL)
  on.exit(options(old), add = TRUE)
  p <- plan_cloud_to_http(plan, st)
  ## nothing known about either bucket: left alone
  expect_identical(p$transport, plan$transport)

  st@cloud <- list(anonymous = TRUE, region = "us-east-1")
  p <- plan_cloud_to_http(plan, st)
  expect_identical(p$transport, c("http", "local", "http", "http"))
  expect_identical(p$path[1], "https://pub.s3.us-east-1.amazonaws.com/a")
  expect_identical(p$path[2:3], plan$path[2:3])
  expect_length(attr(p, "curl_opts"), 4L)
})

test_that("zaro() records the access of an s3:// store", {
  skip_on_cran()
  skip_if_offline("aodn-cloud-optimised.s3.ap-southeast-2.amazonaws.com")
  st <- zaro("s3://aodn-cloud-optimised/model_sea_level_anomaly_gridded_realtime.zarr",
             anonymous = TRUE, verbose = FALSE)
  expect_true(isTRUE(st@cloud$anonymous))
  xy <- zaro_read_arrays(st, c("LATITUDE", "LONGITUDE"), verbose = FALSE)
  expect_gt(attr(xy, "stats")$requests, 0)          # pooled over HTTPS
  expect_equal(range(xy$LATITUDE), c(-60, 10))
})
