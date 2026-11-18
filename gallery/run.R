# Read one small selection from each store in gallery/stores.R and report
# each as ok, unreachable or failed. Run from anywhere, with zaro and its
# Suggests installed:
#
#   Rscript gallery/run.R                       # every store
#   Rscript gallery/run.R cmems-time cmip6-gcs  # some of them
#
# unreachable: a network-level failure (timeout, DNS, refused or reset
#   connection, TLS, HTTP 5xx). The host's problem: reported, and a warning
#   on GitHub Actions, but not a failure.
# failed: anything else, i.e. an error from zaro or a read of the wrong type
#   or shape. Either zaro broke or the store changed (moved, removed, made
#   private, restructured) and its entry needs updating; the note says which.
#   The exit status is 1 if any store failed.
#
# On GitHub Actions the table is also written to the run summary.

suppressPackageStartupMessages(library(zaro))
options(zaro.verbose = FALSE)

script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
here <- if (length(script)) dirname(normalizePath(script[1])) else "gallery"
source(file.path(here, "stores.R"), local = TRUE)

only <- commandArgs(TRUE)
if (length(only)) {
  unknown <- setdiff(only, vapply(stores, `[[`, "", "id"))
  if (length(unknown)) stop("no such store(s): ", paste(unknown, collapse = ", "))
  stores <- Filter(function(s) s$id %in% only, stores)
}

unreachable_pattern <- paste(c(
  "Timeout was reached", "timed out", "[Cc]ould ?n.t resolve", "resolve host",
  "[Ff]ailed to connect", "[Cc]ould ?n.t connect", "[Cc]onnection refused",
  "[Cc]onnection reset", "SSL", "TLS", "HTTP 5[0-9]{2}\\b"), collapse = "|")

fmt_dim <- function(d) paste0(names(d), "=", d, collapse = ", ")

read_store <- function(s) {
  store <- do.call(zaro, c(list(s$source), s$args))
  if (length(s$has)) {
    listed <- names(attr(zaro_meta(store), "consolidated"))
    missing <- setdiff(s$has, listed)
    if (length(missing)) {
      stop("consolidated metadata does not list: ", paste(missing, collapse = ", "))
    }
  }
  x <- zaro_read(store, s$array, start = s$start, count = s$count)
  if (!identical(typeof(x), s$type)) {
    stop("read gave ", typeof(x), ", not ", s$type)
  }
  want <- stats::setNames(as.integer(s$dim), names(s$dim))
  if (!identical(dim(x), want)) {
    stop("read gave dim ", fmt_dim(dim(x)), ", not ", fmt_dim(want))
  }
  note <- character()
  if (isTRUE(s$lazy)) {
    if (requireNamespace("altarr", quietly = TRUE)) {
      a <- zaro_array(store, s$array)
      ## R order is the Zarr order reversed (C order), and 1-based
      idx <- lapply(rev(seq_along(s$start)),
                    function(i) s$start[i] + seq_len(s$count[i]))
      lz <- do.call(altarr::altarr_extract, c(list(a), idx, list(drop = FALSE)))
      if (!isTRUE(all.equal(as.vector(lz), as.vector(aperm(x))))) {
        stop("zaro_array() and zaro_read() differ on the same selection")
      }
      note <- "lazy array agrees"
    } else {
      note <- "altarr not installed: lazy array not checked"
    }
  }
  note
}

check_store <- function(s) {
  warns <- character()
  t0 <- proc.time()[["elapsed"]]
  out <- tryCatch(
    withCallingHandlers(read_store(s), warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }),
    error = function(e) e)
  secs <- proc.time()[["elapsed"]] - t0
  if (inherits(out, "error")) {
    msg <- conditionMessage(out)
    result <- if (grepl(unreachable_pattern, msg)) "unreachable" else "failed"
    note <- msg
  } else {
    result <- "ok"
    note <- paste(c(out, if (length(warns)) paste("warning:", warns)), collapse = "; ")
  }
  note <- gsub("[[:space:]]+", " ", note)
  if (nchar(note) > 300) note <- paste0(substr(note, 1, 297), "...")
  data.frame(id = s$id, exercises = s$exercises, result = result,
             seconds = round(secs, 1), note = note)
}

res <- do.call(rbind, lapply(stores, function(s) {
  r <- check_store(s)
  message(sprintf("%-22s %-11s %6.1fs %s", r$id, r$result, r$seconds, r$note))
  r
}))

cell <- function(x) gsub("|", "\\|", x, fixed = TRUE)
counts <- table(factor(res$result, c("ok", "unreachable", "failed")))
md <- c(sprintf("%d stores: %d ok, %d unreachable, %d failed (%s)",
                nrow(res), counts[["ok"]], counts[["unreachable"]],
                counts[["failed"]], format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
        "",
        "| store | exercises | result | seconds | note |",
        "|---|---|---|---:|---|",
        sprintf("| %s | %s | %s | %.1f | %s |", res$id, cell(res$exercises),
                res$result, res$seconds, cell(res$note)))
cat(md, sep = "\n")
cat("\n")

summary_file <- Sys.getenv("GITHUB_STEP_SUMMARY")
if (nzchar(summary_file)) {
  cat(c("## Store gallery", "", md, ""), file = summary_file, sep = "\n",
      append = TRUE)
}
if (identical(Sys.getenv("GITHUB_ACTIONS"), "true")) {
  for (i in which(res$result != "ok")) {
    level <- if (res$result[i] == "failed") "error" else "warning"
    cat(sprintf("::%s title=gallery %s %s::%s\n", level, res$id[i],
                res$result[i], res$note[i]))
  }
}

quit(status = as.integer(any(res$result == "failed")))
