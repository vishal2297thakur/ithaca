#Validates that the weighted mean of mm agrees with the direct estimate of volumes

source('source/partition_prec.R')
source('source/geo_functions.R')

## Data 
prec_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_datasets.rds"))
prec_grid <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_volume_grid.rds"))
prec_annual <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_global_annual_mean.rds"))

prec_volume <- prec_grid[prec_datasets[, .(lon, lat, prec_mean, dataset)], on = .(lon, lat)]
prec_volume[, prec_volume_year := area * M2_TO_KM2 * prec_mean * MM_TO_KM][, prec_mean := NULL] # km3
prec_annual_vol_mean_dataset <- prec_volume[, sum(prec_volume_year), dataset]

prec_annual_vol <- copy(prec_annual)
prec_annual_vol <- prec_annual_vol[, prec_volume_year := GLOBAL_AREA * M2_TO_KM2 * prec_mean * MM_TO_KM
][, prec_mean := NULL] # km3

prec_annual_vol_mean <- prec_annual_vol[dataset %in% PREC_GLOBAL_DATASETS, .(prec_volume = mean(prec_volume_year, na.rm = TRUE)), dataset]
