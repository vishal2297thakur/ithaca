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

prec_normal <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_gaussian.rds"))

masks_global <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC_SPATIAL,
                               "pRecipe_masks.rds"))

## Analysis
### Normal masks
prec_one <- masks_global[prec_normal[gauss == 1, .(lon, lat)], on = .(lon, lat)]

MIN_N_ELEV <- min(prec_one[!is.na(elev_class), .N, by = .(elev_class)]$N)
MIN_N_KG <- min(prec_one[KG_class_1_name != "Ocean", .N,
                         by = .(KG_class_1_name)]$N)
MIN_N_LAND_COVER <- min(prec_one[land_cover_short_class != "Other", .N,
                                 by = .(land_cover_short_class)]$N)
MIN_N_BIOME <- min(prec_one[biome_short_class != "Water", .N,
                            by = .(biome_short_class)]$N)

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

