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
### Normal masks
prec_data <- prec_data[masks_global[!is.na(country) & country != "Antarctica",
                                    .(lon, lat, country)], on = .(lon, lat)]
prec_data <- prec_data[complete.cases(prec_data)]

prec_data[, median_prec := median(prec, na.rm = TRUE), by = .(lon, lat, date)]

prec_data <- prec_data[, .(rmse_prec = rmse(median_prec, prec), country),
                       .(lon, lat, dataset)] %>% unique()

prec_data <- prec_data[ , .SD[which.min(rmse_prec)], by = .(lon, lat)]
prec_data$dataset <- as.factor(prec_data$dataset)
prec_data[, prec_sident := Mode(dataset)[[1]], by = "country"]

democracy <- unique(prec_data[, .(country, prec_sident)])


## Save
saveRDS(prec_data, paste0(PATH_SAVE_UNCERTAINTY_PREC, "democracy.rds"))
