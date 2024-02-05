# Determine the best data set per category (large memory requirements)

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

prec_one <- masks_global[unique(prec_data[, .(lon, lat)]), on = .(lon,lat)]

registerDoParallel(cores = N_CORES - 1)

## Analysis
### One for all elevation
prec_class_all <- prec_data[prec_one[, .(lon, lat, elev_class)],
                            on = .(lon, lat)]

class_lonlat <- prec_class_all[, .(lon, lat, elev_class)] %>% unique()
class_lonlat[, no_cells := .N, by = "elev_class"]

the_one_elev <- foreach (idx = 1:10000, .combine = rbind) %dopar% {
  lonlat_sample <- split(class_lonlat, by = "elev_class")
  lonlat_sample <- lapply(lonlat_sample, function(x) {
    MIN_N <- unique(x$no_cells)
    x <- x[, .(lon, lat, elev_class)]
    dummie <- x[, .SD[sample(.N, MIN_N%/%2 + 1)], by = elev_class]
    dummie
  })
  lonlat_sample <- rbindlist(lonlat_sample)
  dummie <- prec_class_all[lonlat_sample[, .(lon, lat)], on = .(lon, lat)]
  dummie[, median_prec := median(prec, na.rm = TRUE), by = .(lon, lat, date)]
  dummie <- dummie[, .(rmse_prec = rmse(median_prec, prec), elev_class),
                   .(lon, lat, dataset)] %>% unique()
  dummie <- dummie[ , .SD[which.min(rmse_prec)], by = .(lon, lat)]
  dummie <- dummie[, .(one_for_all = Mode(as.factor(dataset))), .(elev_class)]
  return(dummie)
}

### One for all Koppen Geiger
prec_class_all <- prec_data[prec_one[, .(lon, lat, KG_class_1_name)],
                            on = .(lon, lat)]

class_lonlat <- prec_class_all[, .(lon, lat, KG_class_1_name)] %>% unique()
class_lonlat[, no_cells := .N, by = "KG_class_1_name"]

the_one_kg <- foreach (idx = 1:10000, .combine = rbind) %dopar% {
  lonlat_sample <- split(class_lonlat, by = "KG_class_1_name")
  lonlat_sample <- lapply(lonlat_sample, function(x) {
    MIN_N <- unique(x$no_cells)
    x <- x[, .(lon, lat, KG_class_1_name)]
    dummie <- x[, .SD[sample(.N, MIN_N%/%2 + 1)], by = KG_class_1_name]
    dummie
  })
  lonlat_sample <- rbindlist(lonlat_sample)
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
prec_class_all <- prec_data[prec_one[, .(lon, lat, land_cover_short_class)],
                            on = .(lon, lat)]

class_lonlat <- prec_class_all[, .(lon, lat, land_cover_short_class)] %>%
  unique()
class_lonlat[, no_cells := .N, by = "land_cover_short_class"]

the_one_land_cover <- foreach (idx = 1:10000, .combine = rbind) %dopar% {
  lonlat_sample <- split(class_lonlat, by = "land_cover_short_class")
  lonlat_sample <- lapply(lonlat_sample, function(x) {
    MIN_N <- unique(x$no_cells)
    x <- x[, .(lon, lat, land_cover_short_class)]
    dummie <- x[, .SD[sample(.N, MIN_N%/%2 + 1)], by = land_cover_short_class]
    dummie
  })
  lonlat_sample <- rbindlist(lonlat_sample)
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
prec_class_all <- prec_data[prec_one[, .(lon, lat, biome_short_class)],
                            on = .(lon, lat)]

class_lonlat <- prec_class_all[, .(lon, lat, biome_short_class)] %>% unique()
class_lonlat[, no_cells := .N, by = "biome_short_class"]

the_one_biome <- foreach (idx = 1:10000, .combine = rbind) %dopar% {
  lonlat_sample <- split(class_lonlat, by = "biome_short_class")
  lonlat_sample <- lapply(lonlat_sample, function(x) {
    MIN_N <- unique(x$no_cells)
    x <- x[, .(lon, lat, biome_short_class)]
    dummie <- x[, .SD[sample(.N, MIN_N%/%2 + 1)], by = biome_short_class]
    dummie
  })
  lonlat_sample <- rbindlist(lonlat_sample)
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
     file = paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_one_classes.rda"))
