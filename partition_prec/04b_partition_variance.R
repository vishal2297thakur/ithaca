# Partition precipitation to different regional properties and quantify their uncertainty
source('source/partition_prec.R')

## Data 
prec_annual <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_global_annual_mean.rds"))
prec_grid <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_volume_grid.rds"))

## Variables
GLOBAL_AREA <- 1.330793e+14
prec_annual_vol <- copy(prec_annual)
prec_annual_vol <-   prec_annual_vol[, prec_volume_year := GLOBAL_AREA * M2_TO_KM2 * prec_mean * MM_TO_KM
][, prec_mean := NULL] # km3

prec_annual_vol_mean <- prec_annual_vol[dataset %in% PREC_GLOBAL_DATASETS, .(prec_volume = mean(prec_volume_year, na.rm = TRUE)), dataset]

prec_dataset_sd <- prec_annual[dataset %in% PREC_GLOBAL_DATASETS, .(prec_sd = sd(prec_mean, na.rm = TRUE)), dataset]
prec_dataset_sd[, range(prec_sd)]
prec_dataset_sd[, median(prec_sd)]

prec_annual_sd <- prec_annual[dataset %in% PREC_GLOBAL_DATASETS, .(prec_sd = sd(prec_mean, na.rm = TRUE)), year]
prec_annual_sd <- prec_annual_sd[complete.cases(prec_annual_sd)]
prec_annual_sd[, range(prec_sd)]
prec_annual_sd[, median(prec_sd)]


prec_annual[dataset %in% PREC_GLOBAL_DATASETS, sd(prec_mean, na.rm = TRUE) / 
              mean(prec_mean, na.rm = TRUE), year]
prec_annual[dataset %in% PREC_GLOBAL_DATASETS, sd(prec_mean, na.rm = TRUE) / 
              mean(prec_mean, na.rm = TRUE), dataset]

prec_annual[dataset %in% PREC_GLOBAL_DATASETS, sd(prec_mean, na.rm = TRUE) / 
              mean(prec_mean, na.rm = TRUE), year]
