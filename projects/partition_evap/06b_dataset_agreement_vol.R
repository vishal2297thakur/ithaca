# Estimates stats for dataset agreement classes
source('source/partition_evap.R')

## Data 
source('source/partition_evap.R')
source('source/geo_functions.R')

## Data 
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
evap_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_datasets.rds"))
evap_grid <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_volume_grid.rds"))

## Variables
dataset_agreement <- merge(evap_mask[, .(lon, lat, rel_dataset_agreement)], 
                           evap_grid[, .(lon, lat, area)], 
                           by = c("lon", "lat"))
dataset_agreement <- merge(dataset_agreement, evap_datasets[dataset %in% EVAP_GLOBAL_DATASETS], by = c("lon", "lat"))
dataset_agreement[, evap_volume_year := area * M2_TO_KM2 * evap_mean * MM_TO_KM
][, evap_mean := NULL][, evap_sd := NULL][, area := NULL] # km3


## Analysis
sd(dataset_agreement[rel_dataset_agreement == 'low' | rel_dataset_agreement == 'below average', 
                     sum(evap_volume_year), .(dataset)]$V1)

sd(dataset_agreement[rel_dataset_agreement != 'low' & rel_dataset_agreement != 'below average', 
                     sum(evap_volume_year), .(dataset)]$V1)

dataset_agreement_spread <- dataset_agreement[, .(evap_volume_year = sum(evap_volume_year)), .(rel_dataset_agreement, dataset)]
dataset_agreement_spread <- dataset_agreement_spread[, .(evap_mean = round(mean(evap_volume_year), 0),
                                                         evap_sd = round(sd(evap_volume_year), 0), 
                                                         cv = round(sd(evap_volume_year) / mean(evap_volume_year), 2)), 
                                                     rel_dataset_agreement]
q <- dataset_agreement_spread[c(3, 2, 1, 4, 5)]
