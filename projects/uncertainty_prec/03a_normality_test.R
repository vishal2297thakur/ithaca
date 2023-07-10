# Test for normality on each grid cell (large memory requirements)

source("source/uncertainty_prec.R")

## Data
prec_month <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC,
                             "prec_datasets_month.rds"))
prec_years <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC,
                             "prec_datasets_years.rds"))

## Analysis
prec_month[, n_datasets := .N, .(lon, lat, date)]
prec_month <- prec_month[n_datasets >= MIN_N_DATASETS]

prec_years[, n_datasets := .N, .(lon, lat, date)]
prec_years <- prec_years[n_datasets >= MIN_N_DATASETS]


test_month <- prec_month[, .(sw_p = shapiro.test(prec)$p.value),
                         .(lon, lat, date)]

test_years <- prec_years[, .(sw_p = shapiro.test(prec)$p.value),
                         .(lon , lat, date)]

test_month[, sw := ifelse(sw_p > 0.05, 1, 0)]

test_years[, sw := ifelse(sw_p > 0.05, 1, 0)]

test_month <- test_month[, .(sw = sum(sw)), .(lon, lat)]

test_years <- test_years[, .(sw = sum(sw)), .(lon, lat)]

prec_norm <- merge(test_month, test_years, by = c("lon", "lat"))

prec_norm <- prec_norm[, .(gauss = ifelse(((sw.x >= 192) & (sw.y >= 16)), 1, 0),
                           monex = ifelse(((sw.x >= 192) & (sw.y < 16)), 1, 0),
                           yrsex = ifelse(((sw.x < 192) & (sw.y >= 16)), 1, 0)),
                       .(lon, lat)]

## Save
saveRDS(prec_norm, paste0(PATH_SAVE_UNCERTAINTY_PREC,
                          "prec_gaussian.rds"))
