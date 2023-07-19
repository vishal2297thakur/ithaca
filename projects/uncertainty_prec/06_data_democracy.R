# Test for normality on each grid cell (large memory requirements)

source("source/uncertainty_prec.R")

install.packages(setdiff(c("DescTools", "Metrics"),
                         rownames(installed.packages())))
library(Metrics)
library(DescTools, include.only = "Mode")
## Data
prec_data <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC,
                            "prec_datasets_years.rds"))
prec_data[, n_datasets := .N, .(lon, lat, date)]
prec_data <- prec_data[n_datasets >= MIN_N_DATASETS]

masks_global <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC_SPATIAL,
                               "pRecipe_masks.rds"))

## Analysis
### Countries
prec_country <- prec_data[masks_global[!is.na(country) & country != "Antarctica",
                                    .(lon, lat, country)], on = .(lon, lat)]

prec_country <- prec_country[complete.cases(prec_country)]

prec_country[, median_prec := median(prec, na.rm = TRUE),
             by = .(lon, lat, date)]

prec_country <- prec_country[, .(rmse_prec = rmse(median_prec, prec), country),
                             .(lon, lat, dataset)] %>% unique()

prec_country <- prec_country[ , .SD[which.min(rmse_prec)], by = .(lon, lat)]
prec_country$dataset <- as.factor(prec_country$dataset)
prec_country[, prec_sident := Mode(dataset)[[1]], by = "country"]

prec_country <- unique(prec_country[, .(country, prec_sident)])

### Basins
prec_basins <- prec_data[masks_global[!is.na(basin_id), .(lon, lat, basin_id)],
                         on = .(lon, lat)]

prec_basins <- prec_basins[complete.cases(prec_basins)]

prec_basins[, median_prec := median(prec, na.rm = TRUE), by = .(lon, lat, date)]

prec_basins <- prec_basins[, .(rmse_prec = rmse(median_prec, prec), basin_id),
                           .(lon, lat, dataset)] %>% unique()

prec_basins <- prec_basins[ , .SD[which.min(rmse_prec)], by = .(lon, lat)]
prec_basins$dataset <- as.factor(prec_basins$dataset)
prec_basins[, prec_sident := Mode(dataset)[[1]], by = "basin_id"]

prec_basins <- unique(prec_basins[, .(basin_id, prec_sident)])

## Save
save(prec_country, prec_basins,
     file = paste0(PATH_SAVE_UNCERTAINTY_PREC, "democracy.rds"))
