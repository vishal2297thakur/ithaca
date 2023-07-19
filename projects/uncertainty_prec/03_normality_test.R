# Test for normality on each grid cell

source("source/uncertainty_prec.R")

## Data
prec_data <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC,
                             "prec_datasets_years.rds"))

## Analysis
prec_data[, n_datasets := .N, .(lon, lat, date)]

prec_data <- prec_data[n_datasets >= MIN_N_DATASETS]

test_prec <- prec_data[, .(sw_p = shapiro.test(prec)$p.value),
                       .(lon , lat, date)]

test_prec[, sw := ifelse(sw_p > 0.05, 1, 0)]

test_prec <- test_prec[, .(sw = sum(sw)), .(lon, lat)]


prec_gauss <- test_prec[, .(gauss = ifelse(sw >= 16, 1, 0)), .(lon, lat)]

## Save
saveRDS(prec_gauss, paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_gaussian.rds"))
