#Quick check of data directory consistency and downloaded data content

library(RNetCDF)
library(raster)

em_earth_prcp <- stack('../../shared/data/obs/precip/em_earth.tp.mm.global.195001.201912.025.monthly.nc')

map_scale_size <- 12
n_random_months <- 20

random_months <- sample(1:100, n_random_months)

for(a_month in random_months){
  image(em_earth_prcp[[a_month]],
        col = hcl.colors(map_scale_size, "Blues3", rev = TRUE))
}

era5_cloud <- stack('../../shared/data/sim/cloudcover/global/raw/era5.tcc.frac.global.195901.202112.025.monthly.nc.nc')
for(a_month in random_months){
  image(era5_cloud[[a_month]], 
        col = hcl.colors(map_scale_size, "Blues3"))
}

