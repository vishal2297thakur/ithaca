# Determine the best data set per country/basin (large memory requirements)

source("source/uncertainty_prec.R")

install.packages(setdiff(c("DescTools", "Metrics"),
                         rownames(installed.packages())))
library(Metrics)
library(DescTools, include.only = "Mode")
## Data
prec_data <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC,
                            "prec_datasets_years.rds"))

masks_global <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC_SPATIAL,
                               "pRecipe_masks.rds"))
masks_global <- masks_global[unique(prec_data[, .(lon, lat)]), on = .(lon,lat)]

registerDoParallel(cores = N_CORES - 1)
## Analysis
### Countries
prec_country <- prec_data[masks_global[!is.na(country), .(lon, lat, country)],
                          on = .(lon, lat)]

prec_lonlat <- masks_global[!is.na(country), .(lon, lat, country)]
prec_lonlat[, no_cells := .N, by = "country"]

the_one_country <- foreach (idx = 1:10000, .combine = rbind) %dopar% {
  lonlat_sample <- split(prec_lonlat, by = "country")
  lonlat_sample <- lapply(lonlat_sample, function(x) {
    MIN_N <- unique(x$no_cells)
    x <- x[, .(lon, lat, country)]
    dummie <- x[, .SD[sample(.N, MIN_N%/%2 + 1)], by = country]
    dummie
  })
  lonlat_sample <- rbindlist(lonlat_sample)
  dummie <- prec_country[lonlat_sample[, .(lon, lat)], on = .(lon, lat)]
  dummie[, median_prec := median(prec, na.rm = TRUE), by = .(lon, lat, date)]
  dummie <- dummie[, .(rmse_prec = rmse(median_prec, prec), country),
                   .(lon, lat, dataset)] %>% unique()
  dummie <- dummie[ , .SD[which.min(rmse_prec)], by = .(lon, lat)]
  dummie[, one_for_all := Mode(as.factor(dataset))[[1]], .(country)]
  dummie <- dummie[, .(country, one_for_all)] %>% unique()
  return(dummie)
}

prec_country[, median_prec := median(prec, na.rm = TRUE),
             by = .(lon, lat, date)]

prec_country <- prec_country[, .(rmse_prec = rmse(median_prec, prec), country),
                             .(lon, lat, dataset)] %>% unique()

prec_country <- prec_country[ , .SD[which.min(rmse_prec)], by = .(lon, lat)]
prec_country$dataset <- as.factor(prec_country$dataset)
prec_country[, prec_sident := Mode(dataset)[[1]], by = "country"]

### Basins
prec_basins <- prec_data[masks_global[!is.na(basin_id), .(lon, lat, basin_id)],
                         on = .(lon, lat)]

prec_lonlat <- masks_global[!is.na(basin_id), .(lon, lat, basin_id)]
prec_lonlat[, no_cells := .N, by = "basin_id"]

the_one_basin <- foreach (idx = 1:10000, .combine = rbind) %dopar% {
  lonlat_sample <- split(prec_lonlat, by = "basin_id")
  lonlat_sample <- lapply(lonlat_sample, function(x) {
    MIN_N <- unique(x$no_cells)
    x <- x[, .(lon, lat, basin_id)]
    dummie <- x[, .SD[sample(.N, MIN_N%/%2 + 1)], by = basin_id]
    dummie
  })
  lonlat_sample <- rbindlist(lonlat_sample)
  dummie <- prec_basins[lonlat_sample[, .(lon, lat)], on = .(lon, lat)]
  dummie[, median_prec := median(prec, na.rm = TRUE), by = .(lon, lat, date)]
  dummie <- dummie[, .(rmse_prec = rmse(median_prec, prec), basin_id),
                   .(lon, lat, dataset)] %>% unique()
  dummie <- dummie[ , .SD[which.min(rmse_prec)], by = .(lon, lat)]
  dummie[, one_for_all := Mode(as.factor(dataset))[[1]], .(basin_id)]
  dummie <- dummie[, .(basin_id, one_for_all)] %>% unique()
  return(dummie)
}

prec_basins[, median_prec := median(prec, na.rm = TRUE), by = .(lon, lat, date)]

prec_basins <- prec_basins[, .(rmse_prec = rmse(median_prec, prec), basin_id),
                           .(lon, lat, dataset)] %>% unique()

prec_basins <- prec_basins[ , .SD[which.min(rmse_prec)], by = .(lon, lat)]
prec_basins$dataset <- as.factor(prec_basins$dataset)
prec_basins[, prec_sident := Mode(dataset)[[1]], by = "basin_id"]

## Save
save(prec_country, prec_basins,
     file = paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_country_basin.rda"))

save(the_one_basin, the_one_country,
     file = paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_one_country_basin.rda"))


