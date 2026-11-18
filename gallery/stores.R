# The store gallery: public stores that zaro is checked against weekly
# (gallery/run.R, .github/workflows/gallery.yaml). See gallery/README.md.
#
# One entry per store:
#   id         short name, used in the report and to run some stores only
#   exercises  what this store covers that the others don't
#   source     what zaro() is given; args: other zaro() arguments
#   has        arrays the root's consolidated metadata must list (optional)
#   array, start, count
#              one small read with zaro_read() (0-based start, Zarr order)
#   type, dim  what that read must return: typeof() and dim(), named
#   lazy       also open the array with zaro_array() and compare the same
#              selection (needs altarr; C-order arrays only)
#
# Values are not pinned (published data are revised); type and shape are.

stores <- list(
  list(
    id = "cmems-time",
    exercises = "HTTPS, Zarr V2 consolidated, time-chunked (CMEMS ARCO), lazy array",
    source = paste0("https://s3.waw3-1.cloudferro.com/mdl-arco-time-045/arco/",
                    "SEALEVEL_GLO_PHY_L4_MY_008_047/",
                    "cmems_obs-sl_glo_phy-ssh_my_allsat-l4-duacs-0.125deg_P1D_202411/",
                    "timeChunked.zarr"),
    has = "ugosa",
    array = "ugos", start = c(9989, 400, 500), count = c(2, 5, 6),
    type = "integer", dim = c(time = 2, latitude = 5, longitude = 6),
    lazy = TRUE
  ),
  list(
    id = "cmems-geo",
    exercises = "HTTPS, Zarr V2 consolidated, geo-chunked (CMEMS ARCO): a long time series",
    source = paste0("https://s3.waw3-1.cloudferro.com/mdl-arco-geo-045/arco/",
                    "SEALEVEL_GLO_PHY_L4_MY_008_047/",
                    "cmems_obs-sl_glo_phy-ssh_my_allsat-l4-duacs-0.125deg_P1D_202411/",
                    "geoChunked.zarr"),
    has = "adt",
    array = "ugos", start = c(4000, 400, 500), count = c(200, 1, 1),
    type = "integer", dim = c(time = 200, latitude = 1, longitude = 1)
  ),
  list(
    id = "cmip6-gcs",
    exercises = "gs:// (a known public bucket, anonymous), Pangeo CMIP6",
    source = paste0("gs://cmip6/CMIP6/ScenarioMIP/NOAA-GFDL/GFDL-ESM4/",
                    "ssp585/r1i1p1f1/Omon/zos/gn/v20180701/"),
    has = c("x", "y", "lon", "lat", "zos"),
    array = "zos", start = c(10, 400, 500), count = c(2, 6, 7),
    type = "double", dim = c(time = 2, y = 6, x = 7)
  ),
  list(
    id = "aodn-himawari",
    exercises = "s3:// with region and anonymous, blosc (AODN)",
    source = "s3://aodn-cloud-optimised/satellite_ghrsst_l3c_1day_nighttime_himawari8.zarr",
    args = list(region = "ap-southeast-2", anonymous = TRUE),
    has = c("lat", "lon", "quality_level", "sea_surface_temperature", "sses_count"),
    array = "wind_speed", start = c(10, 1950, 2950), count = c(3, 60, 70),
    type = "double", dim = c(time = 3, lat = 60, lon = 70)
  ),
  list(
    id = "eopf-s2-sharded",
    exercises = "HTTPS, Zarr V3 sharded GeoZarr in nested groups (EOPF Sentinel-2 L2A)",
    source = paste0("https://s3.explorer.eopf.copernicus.eu/esa-zarr-sentinel-explorer-fra/",
                    "tests-output/sentinel-2-l2a/",
                    "S2C_MSIL2A_20260120T084301_N0511_R064_T36UXA_20260120T122910.zarr"),
    array = "measurements/reflectance/r120m/b01", start = c(0, 0), count = c(3, 4),
    type = "double", dim = c(y = 3, x = 4)
  ),
  list(
    id = "bluelink-virtualizarr",
    exercises = "virtualizarr:// Parquet references to NetCDF on NCI THREDDS (BRAN2023)",
    source = paste0("virtualizarr://https://raw.githubusercontent.com/mdsumner/",
                    "virtualized/refs/heads/main/remote/ocean_v_2023.parq"),
    has = c("v", "xu_ocean", "yu_ocean", "st_ocean"),
    array = "v", start = c(10, 5, 550, 950), count = c(3, 2, 6, 7),
    type = "integer", dim = c(Time = 3, st_ocean = 2, yu_ocean = 6, xu_ocean = 7)
  )
)
