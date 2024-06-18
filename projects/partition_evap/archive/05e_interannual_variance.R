

# Estimates interannual variance of global evaporation [km3]
source('source/partition_evap.R')

## Data 
evap_annual_vol <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_global_annual_mean.rds"))
evap_grid <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_volume_grid.rds"))
evap_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_datasets.rds"))

## Variables
evap_datasets_area <- merge(evap_datasets[, .(lon, lat, dataset)], 
                    evap_grid[, .(lon, lat, area)], by = c("lon", "lat"), all = TRUE)
global_area_dataset <- evap_datasets_area[, .(area = sum(area)), .(dataset)] 
evap_annual_vol <- evap_annual_vol[global_area_dataset, on = .(dataset)]

evap_annual_vol <- evap_annual_vol[, evap_volume_year := area  * M2_TO_KM2 * evap_mean * MM_TO_KM
][, evap_mean := NULL][, area := NULL] # km3

evap_annual_vol[dataset %in% EVAP_GLOBAL_DATASETS, 
                                        .(evap_volume = mean(evap_volume_year, na.rm = TRUE)), dataset]

evap_dataset_sd <- evap_annual_vol[dataset %in% EVAP_GLOBAL_DATASETS, 
                                   .(evap_sd = sd(evap_volume_year, na.rm = TRUE)), dataset]
evap_dataset_sd[, range(evap_sd)]
evap_dataset_sd[, median(evap_sd)]

evap_annual_sd <- evap_annual_vol[dataset %in% EVAP_GLOBAL_DATASETS, 
                                  .(evap_sd = sd(evap_volume_year, na.rm = TRUE)), year]
evap_annual_sd <- evap_annual_sd[complete.cases(evap_annual_sd)]
evap_annual_sd[, range(evap_sd)]
evap_annual_sd[, mean(evap_sd)]

