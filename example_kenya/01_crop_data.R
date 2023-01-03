### Reading the data and extracting the data for the specified region for a period of 60 years

source('source/example_kenya.R')
source('source/geo_functions.R')

fname_prec_era5 <- list.files(path = path_prec, full.names = T, pattern = "era5_tp*") 
fname_prec_terra <- list.files(path = path_prec, full.names = T, pattern = "terraclimate_tp*") 

fname_evap_era5 <- list.files(path = path_evap, full.names = T, pattern = "era5_e_*") 
fname_evap_terra <- list.files(path = path_evap, full.names = T, pattern = "terraclimate_e_*") 

# Read data
prec_era5 <- brick(fname_prec_era5)
prec_terra <- brick(fname_prec_terra)

evap_era5 <- brick(fname_evap_era5)
evap_terra <- brick(fname_evap_terra)

# Subset data over study area and period
prec_era5_kenya <- crop_space_time(prec_era5, PERIOD_START, PERIOD_END, study_area)
prec_terra_kenya <- crop_space_time(prec_terra, PERIOD_START, PERIOD_END, study_area)

evap_era5_kenya <- crop_space_time(evap_era5, PERIOD_START, PERIOD_END, study_area)
evap_terra_kenya <- crop_space_time(evap_terra, PERIOD_START, PERIOD_END, study_area)

# Remove global data
rm(prec_era5); rm(prec_terra)
rm(evap_era5); rm(evap_terra)
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