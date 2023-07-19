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
masks_global <- masks_global[unique(prec_data[, .(lon, lat)]), on = .(lon,lat)]

prec_data <- merge(prec_data, masks_global, by = c("lon", "lat"))

prec_elev <- split(prec_data[, .(lon, lat, date, dataset, value = prec, elev_class)],
                   by = c("dataset", "elev_class"))

prec_elev <- foreach (idx = 1:length(prec_elev), .combine = rbind) %do% {
  dummie <- prec_elev[[idx]]
  dummie_data <- unique(dummie$dataset)
  dummie_elev <- unique(dummie$elev_class)
  dummie <- fldmean(dummie[, .(lon, lat, date, value)])
  dummie$dataset <- dummie_data
  dummie$elev_class <- dummie_elev
  return(dummie)
}

registerDoParallel(cores = N_CORES - 1)

### One for all elevation
prec_class_all <- prec_data[prec_one[!is.na(elev_class),
                                     .(lon, lat, elev_class)], on = .(lon, lat)]

class_lonlat <- prec_class_all[, .(lon, lat, elev_class)] %>% unique()

the_one_elev <- foreach (idx = 1:10000, .combine = rbind) %dopar% {
  lonlat_sample <- class_lonlat[, .SD[sample(.N, MIN_N_ELEV%/%2 + 1)],
                                by = elev_class]
  
  dummie <- prec_class_all[lonlat_sample[, .(lon, lat)], on = .(lon, lat)]
  dummie[, median_prec := median(prec, na.rm = TRUE), by = .(lon, lat, date)]
  dummie <- dummie[, .(rmse_prec = rmse(median_prec, prec), elev_class),
                   .(lon, lat, dataset)] %>% unique()
  dummie <- dummie[ , .SD[which.min(rmse_prec)], by = .(lon, lat)]
  dummie <- dummie[, .(one_for_all = Mode(as.factor(dataset))), .(elev_class)]
  return(dummie)
}

### One for all Koppen Geiger
prec_class_all <- prec_data[prec_one[KG_class_1_name != "Ocean",
                                     .(lon, lat, KG_class_1_name)],
                            on = .(lon, lat)]

class_lonlat <- prec_class_all[, .(lon, lat, KG_class_1_name)] %>% unique()

the_one_kg <- foreach (idx = 1:10000, .combine = rbind) %dopar% {
  lonlat_sample <- class_lonlat[, .SD[sample(.N, MIN_N_KG%/%2 + 1)],
                                by = KG_class_1_name]
  
  dummie <- prec_class_all[lonlat_sample[, .(lon, lat)], on = .(lon, lat)]
  dummie[, median_prec := median(prec, na.rm = TRUE), by = .(lon, lat, date)]
  dummie <- dummie[, .(rmse_prec = rmse(median_prec, prec), KG_class_1_name),
                   .(lon, lat, dataset)] %>% unique()
  dummie <- dummie[ , .SD[which.min(rmse_prec)], by = .(lon, lat)]
  dummie <- dummie[, .(one_for_all = Mode(as.factor(dataset))),
                   .(KG_class_1_name)]
  return(dummie)
}

### One for all land cover
prec_class_all <- prec_data[prec_one[land_cover_short_class != "Other",
                                     .(lon, lat, land_cover_short_class)],
                            on = .(lon, lat)]

class_lonlat <- prec_class_all[, .(lon, lat, land_cover_short_class)] %>%
  unique()

the_one_land_cover <- foreach (idx = 1:10000, .combine = rbind) %dopar% {
  lonlat_sample <- class_lonlat[, .SD[sample(.N, MIN_N_LAND_COVER%/%2 + 1)],
                                by = land_cover_short_class]
  
  dummie <- prec_class_all[lonlat_sample[, .(lon, lat)], on = .(lon, lat)]
  dummie[, median_prec := median(prec, na.rm = TRUE), by = .(lon, lat, date)]
  dummie <- dummie[, .(rmse_prec = rmse(median_prec, prec),
                       land_cover_short_class),
                   .(lon, lat, dataset)] %>% unique()
  dummie <- dummie[ , .SD[which.min(rmse_prec)], by = .(lon, lat)]
  dummie <- dummie[, .(one_for_all = Mode(as.factor(dataset))),
                   .(land_cover_short_class)]
  return(dummie)
}

### One for all biome
prec_class_all <- prec_data[prec_one[biome_short_class != "Water",
                                     .(lon, lat, biome_short_class)],
                            on = .(lon, lat)]

class_lonlat <- prec_class_all[, .(lon, lat, biome_short_class)] %>% unique()

the_one_biome <- foreach (idx = 1:10000, .combine = rbind) %dopar% {
  lonlat_sample <- class_lonlat[, .SD[sample(.N, MIN_N_BIOME%/%2 + 1)],
                                by = biome_short_class]
  
  dummie <- prec_class_all[lonlat_sample[, .(lon, lat)], on = .(lon, lat)]
  dummie[, median_prec := median(prec, na.rm = TRUE), by = .(lon, lat, date)]
  dummie <- dummie[, .(rmse_prec = rmse(median_prec, prec), biome_short_class),
                   .(lon, lat, dataset)] %>% unique()
  dummie <- dummie[ , .SD[which.min(rmse_prec)], by = .(lon, lat)]
  dummie <- dummie[, .(one_for_all = Mode(as.factor(dataset))),
                   .(biome_short_class)]
  return(dummie)
}

## Save
save(the_one_elev, the_one_kg, the_one_land_cover, the_one_biome,
     file = paste0(PATH_SAVE_UNCERTAINTY_PREC, "one_for_all.rda"))

