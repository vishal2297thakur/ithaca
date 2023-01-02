source('source/example_kenya.R')

### Reading the data and extracting the data for the specified region crop_box for a period of 60 years (12x60 months)

fname_prec_era5 <- list.files(path = path_prec, full.names = T, pattern = "era5_tp*") 
fname_prec_terra <- list.files(path = path_prec, full.names = T, pattern = "terraclimate_tp*") 
fname_prec_mswep <- list.files(path = path_prec, full.names = T, pattern = "mswep_tp*") 
fname_prec_gpcp <- list.files(path = path_prec, full.names = T, pattern = "gpcp_tp*") 

## Precipitation

# Read data
prec_era5 <- brick(fname_prec_era5)
prec_terra <- brick(fname_prec_terra)

# Crop data (Kenya) and filter to time
time_filter_era5 <- which(getZ(prec_era5) >= period_start & 
                       (getZ(prec_era5) <= period_end))
time_filter_terra <- which(getZ(prec_terra) >= period_start & 
                             (getZ(prec_terra) <= period_end))

prec_era5_kenya <- crop(prec_era5, crop_box)
prec_era5_kenya <- subset(prec_era5_kenya, time_filter_era5)

prec_terra_kenya <- crop(prec_terra, crop_box)
prec_terra_kenya <- subset(prec_terra_kenya, time_filter_terra)

# Remove global data
rm(prec_era5)
rm(prec_terra)
gc()


## Evapotranspiration

# Read data
evap_era5 <- brick(fname_evap_era5)
evap_terra <- brick(fname_evap_terra)

# Crop data (Kenya) and filter to time
evap_era5_kenya <- crop(evap_era5, crop_box)
evap_era5_kenya <- subset(evap_era5_kenya, time_filter_era5)

evap_terra_kenya <- crop(evap_terra, crop_box)
evap_terra_kenya <- subset(evap_terra_kenya, time_filter_terra)

# Remove global data
rm(evap_era5)
rm(evap_terra)
gc()

# Quick validation
plot(prec_era5_kenya[[1:12]])
plot(prec_terra_kenya[[1:12]])
plot(evap_era5_kenya[[1:12]])
plot(evap_terra_kenya[[1:12]])

# Save data for further use
saveRDS(prec_era5_kenya, paste0(path_save_kenya, "prec_era5.rds"))
saveRDS(prec_terra_kenya, paste0(path_save_kenya, "prec_terra.rds"))
saveRDS(evap_era5_kenya, paste0(path_save_kenya, "evap_era5.rds"))
saveRDS(evap_terra_kenya, paste0(path_save_kenya, "evap_terra.rds"))