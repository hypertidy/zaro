skip_if_not(Sys.getenv("ZARO_GALLERY_TESTS") == "YES")
## do we really need curl for this
offline <- function() {
  tst <- try(readLines("https://raw.githubusercontent.com/cran/MASS/refs/heads/master/DESCRIPTION", n = 1))
  if (inherits(tst, "try-error")) return(TRUE)
  FALSE
}

skip_if(offline())


test_that("Pangeo GPCP via HTTP", {
  store <- zaro("https://ncsa.osn.xsede.org/Pangeo/pangeo-forge/gpcp-feedstock/gpcp.zarr")
  meta <- zaro_meta(store)
  expect_true("precip" %in% names(attr(meta, "consolidated")))
  data <- zaro_read(store, "precip", start = c(100, 0, 0), count = c(1, 2, 2))
  expect_equal(dim(data), c(time = 1, latitude = 2, longitude = 2))
})

test_that("CMEMS ARCO via HTTP (timeChunked)", {
  store <- zaro("https://s3.waw3-1.cloudferro.com/mdl-arco-time-045/arco/SEALEVEL_GLO_PHY_L4_MY_008_047/cmems_obs-sl_glo_phy-ssh_my_allsat-l4-duacs-0.125deg_P1D_202411/timeChunked.zarr")

  meta <- zaro_meta(store)
  expect_true("ugosa" %in% names(attr(meta, "consolidated")))


  data <- zaro_read(store, "ugos", start = c(9989, 400, 500), count = c(2, 5, 6))
  expect_true(typeof(data) == "integer")
  expect_equal(dim(data), c(time = 2L, latitude = 5L, longitude = 6L))

})


test_that("CMEMS ARCO via HTTP (timeChunked, with parallel)", {
  store <- zaro("https://s3.waw3-1.cloudferro.com/mdl-arco-time-045/arco/SEALEVEL_GLO_PHY_L4_MY_008_047/cmems_obs-sl_glo_phy-ssh_my_allsat-l4-duacs-0.125deg_P1D_202411/timeChunked.zarr")

  meta <- zaro_meta(store)
  ugos <- attr(meta, "consolidated")$ugos
  future::plan(future.mirai::mirai_multisession, workers = 2)
  expect_warning(
    data <- zaro_read(store, "ugos", start = c(9989, 400, 500), count = c(2, 1200, 1800), parallel = TRUE, assemble = FALSE),
                 "clamping")


  # 1440 - 400 = 1040, 2880 - 500 = 2380 (or could be NA)
  data <- zaro_read(store, "ugos", start = c(9989, 400, 500),
                    count = c(2, 1040, 2380), parallel = TRUE, assemble = FALSE)

  expect_true(typeof(data) == "list")
  expect_equal(length(data), 18L)
  expect_named(data[[1L]], c("cidx", "values"))
  expect_equal(length(data[[1]]$cidx), 3L)
  expect_equal(length(data[[1]]$values), prod(ugos@chunk_shape))
  non_null <- Filter(Negate(is.null), data)
  expect_true(length(non_null) >= 18L)  # all chunks present
  expect_named(non_null[[1L]], c("cidx", "values"))

  future::plan(future::sequential)
})

test_that("CMEMS ARCO via HTTP (geoChunked)", {
  store <- zaro("https://s3.waw3-1.cloudferro.com/mdl-arco-geo-045/arco/SEALEVEL_GLO_PHY_L4_MY_008_047/cmems_obs-sl_glo_phy-ssh_my_allsat-l4-duacs-0.125deg_P1D_202411/geoChunked.zarr")
  meta <- zaro_meta(store)
  expect_true("adt" %in% names(attr(meta, "consolidated")))

  data <- zaro_read(store, "ugos", start = c(4000, 400, 500), count = c(200, 1, 1))
  expect_true(typeof(data) == "integer")
  expect_equal(dim(data), c(time = 200L, latitude = 1L, longitude = 1L))

})

test_that("GS CMIP6 ARCO via gs://", {
  store <- zaro("gs://cmip6/CMIP6/ScenarioMIP/NOAA-GFDL/GFDL-ESM4/ssp585/r1i1p1f1/Omon/zos/gn/v20180701/")
  meta <- zaro_meta(store)
  expect_true(all(c("x", "y", "lon", "lat", "zos") %in% names(attr(meta, "consolidated"))))

  data <- zaro_read(store, "zos", start = c(10, 400, 500), count = c(2, 6, 7))
  expect_true(typeof(data) == "double")
  expect_equal(dim(data), c(time = 2L, y = 6L, x = 7L))

})

test_that("AODN Himawari via s3:// with region", {
  store <- zaro("s3://aodn-cloud-optimised/satellite_ghrsst_l3c_1day_nighttime_himawari8.zarr", region = "ap-southeast-2", anonymous = TRUE)
  meta <- zaro_meta(store)
  expect_true(all(c("lat", "lon", "quality_level", "sea_surface_temperature", "sses_count") %in% names(attr(meta, "consolidated"))))

  data <- zaro_read(store, "wind_speed", start = c(10, 1950, 2950), count = c(3, 60, 70))
  expect_true(typeof(data) == "double")
  expect_equal(dim(data), c(time = 3L, lat = 60L, lon = 70L))

  ## bigger test for interactive planning
  # future::plan(future.mirai::mirai_multisession, workers = 24)
  #  data <- zaro_read(store, "wind_speed", start = c(0, 0, 0), count = c(1, NA, NA))
  # [zaro] checking .zmetadata for 'wind_speed'
  # [zaro] found 'wind_speed' in consolidated metadata
  # [zaro]   dtype: float64 | shape: [2656, 4500, 6000] | chunks: [5, 500, 500]
  # [zaro]   codecs: bytes -> blosc
  # [zaro]   dimensions: time, lat, lon
  # [zaro] reading 108 chunk(s) for path 'wind_speed' (V2)
  #
})

test_that("VirtualiZarr Parquet via HTTP to NCI Thredds", {

  # bluelink_parq <-c("atm_flux_diag_2023.parq", "ice_force_2023.parq", "ocean_eta_t_2023.parq",
  #                 "ocean_force_2023.parq", "ocean_mld_2023.parq", "ocean_salt_2023.parq",
  #                 "ocean_temp_2023.parq", "ocean_tx_trans_int_z_2023.parq", "ocean_ty_trans_int_z_2023.parq",
  #                 "ocean_u_2023.parq", "ocean_v_2023.parq", "ocean_w_2023.parq")
 parq <- "ocean_v_2023.parq"
 remote <- "virtualizarr://https://raw.githubusercontent.com/mdsumner/virtualized/refs/heads/main/remote"
 src <- paste(remote, parq, sep = "/")
 store <- zaro(src)
 meta <- zaro_meta(store)
 expect_true(all(c("v", "xu_ocean", "yu_ocean", "st_ocean") %in% names(attr(meta, "consolidated"))))

 data <- zaro_read(store, "v", start = c(10, 5, 550, 950), count = c(3, 2, 6, 7))
 expect_true(typeof(data) == "integer")
 expect_equal(dim(data), c(Time = 3L, st_ocean = 2L, yu_ocean = 6L, xu_ocean = 7L))

})

