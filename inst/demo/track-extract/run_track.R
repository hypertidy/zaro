# run_track.R -- extract a short 4D track of BRAN2023 temperature
#
#   Rscript run_track.R            # real run against THREDDS (needs access)
#   Rscript run_track.R mock 8765  # dry run: refs from GitHub, chunk bytes
#                                  # from mock_thredds.py on that port
#
# Needs: zaro (hypertidy/zaro, with zaro_array), altarr, curl, jsonlite,
# and arrow or nanoparquet for the manifest shards. ASCII only.

args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args)) args[1] else "real"
port <- if (length(args) > 1) args[2] else "8765"
max_con <- as.integer(Sys.getenv("TRACK_MAX_CON", "8"))
here <- dirname(sub("^--file=", "",
                    grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))
source(file.path(here, "track_extract.R"))

## a made-up 30 day track off eastern Tasmania, diving deeper over time
n <- 30
track <- data.frame(
  time  = as.POSIXct("2023-03-01 12:00", tz = "UTC") + (seq_len(n) - 1) * 86400,
  lon   = seq(147.8, 152.5, length.out = n),
  lat   = seq(-43.5, -39.0, length.out = n),
  depth = round(5 + 195 * (1 - cos(seq(0, pi, length.out = n))) / 2)
)

t0 <- Sys.time()
store <- open_bran("ocean_temp_2023")
coords <- lapply(setNames(nm = c("xt_ocean", "yt_ocean", "st_ocean", "Time")),
                 function(v) bran_coord(store, v))
attrs <- bran_attrs(store, "temp")
t_meta <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

hook <- function(url, zidx) url
if (mode == "mock") {
  hook <- function(url, zidx) {
    sprintf("http://127.0.0.1:%s/mock?c=%s&f=%s", port,
            paste(zidx, collapse = "."), basename(url))
  }
}
x <- bran_array(store, "temp", max_con = max_con, url_hook = hook)
cat("temp R dims:", paste(names(dimnames(x)), dim(x), collapse = ", "), "\n")

idx <- track_index(coords, track$lon, track$lat, track$depth, track$time)
track_stats_reset()
t1 <- Sys.time()
cs <- c(300L, 300L, 1L, 1L)
plan <- unique(sweep(idx - 1L, 2, cs, `%/%`))
t_plan <- as.numeric(difftime(Sys.time(), t1, units = "secs"))
t1 <- Sys.time()
track$temp <- track_extract(x, idx, attrs)
t_fetch <- as.numeric(difftime(Sys.time(), t1, units = "secs"))

track$i <- idx[, "x"]; track$j <- idx[, "y"]
track$k <- idx[, "z"]; track$l <- idx[, "t"]
print(track, digits = 4, row.names = FALSE)

s <- track_stats()
a <- as.list(altarr_stats(x))
cat(sprintf(paste0(
  "\nmode=%s max_con=%d\n",
  "metadata + coords + shard: %.2f s\n",
  "chunks planned: %d, altarr fetch calls: %d\n",
  "byte-range requests to data server: %d in %d batch(es), %.1f KB, %.2f s\n",
  "track extract wall time: %.2f s\n"),
  mode, max_con, t_meta, nrow(plan), a$fetch_calls, s$requests, s$batches,
  s$bytes / 1024, s$seconds, t_fetch))

if (mode == "mock") {
  # the mock server encodes v = (x + 3y + 5z + 7t) %% 30000 (0-based)
  raw_expect <- ((idx[, 1] - 1) + 3 * (idx[, 2] - 1) + 5 * (idx[, 3] - 1) +
                   7 * (idx[, 4] - 1)) %% 30000
  got <- (track$temp - attrs$add_offset) / attrs$scale_factor
  cat("mock check, values match expected indices:",
      isTRUE(all.equal(round(got), raw_expect)), "\n")
  srv <- jsonlite::fromJSON(rawToChar(
    curl::curl_fetch_memory(sprintf("http://127.0.0.1:%s/stats", port))$content))
  cat(sprintf("mock server saw %d requests, peak concurrency %d\n",
              srv$requests, srv$peak))
}

if (mode == "real") {
  write.csv(track, file.path(here, "track_real.csv"), row.names = FALSE)
}
