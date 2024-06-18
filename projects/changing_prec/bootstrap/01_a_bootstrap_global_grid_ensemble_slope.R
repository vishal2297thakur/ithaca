# Trend (Theil-Sen Slope) for each grid for global ensemble of all data products with bootstrap----
source('source/changing_prec.R')
source('source/geo_functions.R')

library(openair)

## Data ----
PATH_SAVE_CHANGING_PREC <- paste0(PATH_SAVE, "changing_prec/")
prec_datasets <- readRDS(paste0(PATH_SAVE_CHANGING_PREC, "prec_datasets.rds"))

## Analysis ----
prec_datasets[, year := as.numeric(as.character(year))]
prec_datasets_grid_mean <- prec_datasets[, .(prec = mean(prec, na.rm = T), count = .N), .(lat, lon, year)]
prec_datasets_grid_mean <- prec_datasets_grid_mean[count >= 10]
prec_datasets_grid_mean[, date := paste0(year, "-01-01 00:00:00")]
prec_datasets_grid_mean[, date := as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")]
mean_prec <- prec_datasets_grid_mean[, .(mean_prec = mean(prec, na.rm = T)), .(lat, lon)]

prec_trend_grid_ensemble <- prec_trends_lon_lat_boot(prec_datasets_grid_mean)

prec_trend_grid_ensemble[p > 0.05, significant_theil_sen := FALSE] 
prec_trend_grid_ensemble[p <= 0.05, significant_theil_sen := TRUE] 

prec_trend_grid_ensemble <- merge(prec_trend_grid_ensemble, mean_prec, by = c("lon", "lat"))
prec_trend_grid_ensemble[, slope_percent := slope/mean_prec*100]

## Save ----
saveRDS(prec_trend_grid_ensemble, paste0(PATH_SAVE_CHANGING_PREC, "prec_trend_ensemble_grid_bootstrap.rds"))  
