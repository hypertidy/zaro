# ghrsst-scratch.R -- GHRSST MUR SST through a draft virtual index
#
# EXPLORATORY. The index read here is a draft in the source.coop scratch
# product (ausantarctic/ausantarctic-scratch/ghrsst/), built by
# raad.virtualize. It will move when it is folded into production, and until
# then its references can lag the COGs: a COG rewritten after the index was
# built (recent near-real-time days) has stale byte ranges, and zaro reports
# that chunk as failing to decode. Only the COGs and their mdim.vrt are
# published (ausantarctic/ghrsst-mur-v2).
#
# What it shows:
#   - a kerchunk Parquet index over 8920 daily COGs (MUR 0.01 degree global
#     SST), opened with zaro; it uses the fsspec directory layout, which
#     zaro reads with virtualizarr://
#   - TIFF tiles: zstd with horizontal differencing (TIFF predictor 2,
#     codec imagecodecs_delta)
#   - overview levels as arrays (analysed_sst_x2 ... _x32, byterefs
#     decision 0006): a whole-globe day at 1/32 resolution is a few tiles
#   - a full-resolution window, checked against GDAL reading the COG
#     (needs gdalraster)
#
#   Rscript ghrsst-scratch.R
#
# ASCII only.

library(zaro)
options(zaro.verbose = FALSE)

index <- paste0("virtualizarr://https://data.source.coop/ausantarctic/",
                "ausantarctic-scratch/ghrsst/ghrsst-mur-cog-analysed_sst/kerchunk")
store <- zaro(index)
ds <- zaro_dataset(store)
print(ds)

time <- as.Date("1970-01-01") + as.vector(zaro_read(store, "time", meta = ds$arrays$time))
## stay clear of the most recent days, whose COGs may postdate the index
day <- length(time) - 20L
cat("day:", format(time[day]), "\n")

## the whole globe at 1/32 resolution (0.32 degree): one plan, a few tiles
p32 <- zaro_plan(ds, "analysed_sst_x32", start = c(day - 1, 0, 0), count = c(1, NA, NA))
print(p32)
sst32 <- zaro_array(ds, "analysed_sst_x32", cf = TRUE)     # x_x32, y_x32, time
g <- sst32[, , day]
x32 <- as.vector(zaro_read(store, "x_x32", meta = ds$arrays$x_x32))
y32 <- as.vector(zaro_read(store, "y_x32", meta = ds$arrays$y_x32))
image(x32, rev(y32), g[, ncol(g):1], asp = 1, useRaster = TRUE,
      col = hcl.colors(64, "Spectral", rev = TRUE),
      xlab = "longitude", ylab = "latitude",
      main = paste("GHRSST MUR SST (degC), 1/32 overview,", format(time[day])))

## full resolution: a 1000 x 1000 window off south-east Australia
st <- c(day - 1, 13000, 33000); ct <- c(1, 1000, 1000)
w <- zaro_read(store, "analysed_sst", start = st, count = ct,
               meta = ds$arrays$analysed_sst)[1, , ]
cat("window: SST", paste(round(range(w[w != -32768]) * 0.001 + 25, 2), collapse = " to "),
    "degC\n")

## the same window from the COG itself, through GDAL
if (requireNamespace("gdalraster", quietly = TRUE)) {
  p <- zaro_plan(store, "analysed_sst", start = st, count = ct)
  cog <- new(gdalraster::GDALRaster, paste0("/vsicurl/", p$path[1]))
  gv <- matrix(cog$read(band = 1, xoff = st[3], yoff = st[2], xsize = ct[3], ysize = ct[2],
                        out_xsize = ct[3], out_ysize = ct[2]), ct[2], ct[3], byrow = TRUE)
  gv[is.na(gv)] <- -32768                 # gdalraster reads nodata as NA
  cat("identical to GDAL reading the COG:", all(as.vector(w) == as.vector(gv)), "\n")
  cog$close()
}
