zaro_meta(store <- zaro("https://ncsa.osn.xsede.org/Pangeo/pangeo-forge/gpcp-feedstock/gpcp.zarr"))
str(zaro_read(store, "precip", start = c(100, 0, 0), count = c(1, NA, NA)))

zaro_meta(store <- zaro("https://s3.waw3-1.cloudferro.com/mdl-arco-time-045/arco/SEALEVEL_GLO_PHY_L4_MY_008_047/cmems_obs-sl_glo_phy-ssh_my_allsat-l4-duacs-0.125deg_P1D_202411/timeChunked.zarr"))
str(zaro_read(store, "ugos", start = c(0, 0, 0), count = c(1, NA, NA)))

zaro_meta(store <- zaro("https://s3.waw3-1.cloudferro.com/mdl-arco-geo-045/arco/SEALEVEL_GLO_PHY_L4_MY_008_047/cmems_obs-sl_glo_phy-ssh_my_allsat-l4-duacs-0.125deg_P1D_202411/geoChunked.zarr"))
str(zaro_read(store, "ugos", start = c(0, 100, 1000), count = c(100, 16, 16)))

zaro_meta(store <- zaro("gs://cmip6/CMIP6/ScenarioMIP/NOAA-GFDL/GFDL-ESM4/ssp585/r1i1p1f1/Omon/zos/gn/v20180701/"))
str(m <- zaro_read(store, "zos", start = c(1000, 0, 0), count = c(1, NA, NA)))
range(m, na.rm = TRUE)

zaro_meta(store <- zaro("s3://aodn-cloud-optimised/satellite_ghrsst_l3c_1day_nighttime_himawari8.zarr", region = "ap-southeast-2", anonymous = TRUE))
str(m <- zaro_read(store, "wind_speed", start = c(1000, 0, 0), count = c(1, NA, NA)))
range(m, na.rm = TRUE)

bluelink_parq <-c("atm_flux_diag_2023.parq", "ice_force_2023.parq", "ocean_eta_t_2023.parq",
                  "ocean_force_2023.parq", "ocean_mld_2023.parq", "ocean_salt_2023.parq",
                  "ocean_temp_2023.parq", "ocean_tx_trans_int_z_2023.parq", "ocean_ty_trans_int_z_2023.parq",
                  "ocean_u_2023.parq", "ocean_v_2023.parq", "ocean_w_2023.parq")
remote <- "virtualizarr://https://raw.githubusercontent.com/mdsumner/virtualized/refs/heads/main/remote"
src <- paste(remote, bluelink_parq[7], sep = "/")
store <- zaro(src)
library(future)
plan(future.mirai::mirai_multisession, workers = 8)
system.time(m <- zaro_read(store, "temp", start = c(0, 0, 0, 0), count = c(1, 2, NA, NA)))
dim(m)


