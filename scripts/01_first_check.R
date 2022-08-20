library(RNetCDF)
library(raster)

em_earth_prcp <- stack('../../shared/data/obs/precip/em_earth.tp.mm.global.195001.201912.025.monthly.nc')
for(i in 1:12){
  image(em_earth_prcp[[i]],
        col = hcl.colors(12, "Blues3", rev = TRUE))
}

era5_cloud <- stack('../../shared/data/sim/cloudcover/global/raw/era5.tcc.frac.global.195901.202112.025.monthly.nc.nc')
for(i in 1:12){
  image(era5_cloud[[i]], 
        col = hcl.colors(12, "Blues3"))
}

