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

masks_global <- masks_global[unique(prec_data[, .(lon, lat)]), on = .(lon,lat)]

prec_data <- merge(prec_data, masks_global, by = c("lon", "lat"))

## Analysis
### Elevation
prec_elev <- split(prec_data[, .(lon, lat, date, dataset,
                                 value = prec, elev_class)],
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

### Koppen Geiger
prec_kg <- split(prec_data[, .(lon, lat, date, dataset,
                               value = prec, KG_class_1_name)],
                 by = c("dataset", "KG_class_1_name"))

prec_kg <- foreach (idx = 1:length(prec_kg), .combine = rbind) %do% {
  dummie <- prec_kg[[idx]]
  dummie_data <- unique(dummie$dataset)
  dummie_kg <- unique(dummie$KG_class_1_name)
  dummie <- fldmean(dummie[, .(lon, lat, date, value)])
  dummie$dataset <- dummie_data
  dummie$KG_class_1_name <- dummie_kg
  return(dummie)
}

### Land cover
prec_land <- split(prec_data[, .(lon, lat, date, dataset,
                                 value = prec, land_cover_short_class)],
                   by = c("dataset", "land_cover_short_class"))

prec_land <- foreach (idx = 1:length(prec_land), .combine = rbind) %do% {
  dummie <- prec_land[[idx]]
  dummie_data <- unique(dummie$dataset)
  dummie_land <- unique(dummie$land_cover_short_class)
  dummie <- fldmean(dummie[, .(lon, lat, date, value)])
  dummie$dataset <- dummie_data
  dummie$land_cover_short_class <- dummie_land
  return(dummie)
}

### Biome
prec_biome <- split(prec_data[, .(lon, lat, date, dataset,
                                  value = prec, biome_short_class)],
                   by = c("dataset", "biome_short_class"))

prec_biome <- foreach (idx = 1:length(prec_biome), .combine = rbind) %do% {
  dummie <- prec_biome[[idx]]
  dummie_data <- unique(dummie$dataset)
  dummie_biome <- unique(dummie$biome_short_class)
  dummie <- fldmean(dummie[, .(lon, lat, date, value)])
  dummie$dataset <- dummie_data
  dummie$biome_short_class <- dummie_biome
  return(dummie)
}

### Countries
prec_country <- split(prec_data[, .(lon, lat, date, dataset,
                                    value = prec, country)],
                      by = c("dataset", "country"))

prec_country <- foreach (idx = 1:length(prec_country), .combine = rbind) %do% {
  dummie <- prec_country[[idx]]
  dummie_data <- unique(dummie$dataset)
  dummie_country <- unique(dummie$country)
  dummie <- fldmean(dummie[, .(lon, lat, date, value)])
  dummie$dataset <- dummie_data
  dummie$country <- dummie_country
  return(dummie)
}

### Basins
prec_basins <- split(prec_data[, .(lon, lat, date, dataset,
                                   value = prec, basin_id)],
                     by = c("dataset", "basin_id"))

prec_basins <- foreach (idx = 1:length(prec_basins), .combine = rbind) %do% {
  dummie <- prec_basins[[idx]]
  dummie_data <- unique(dummie$dataset)
  dummie_basin <- unique(dummie$basin_id)
  dummie <- fldmean(dummie[, .(lon, lat, date, value)])
  dummie$dataset <- dummie_data
  dummie$basin_id <- dummie_basin
  return(dummie)
}

## Save
save(the_one_elev, the_one_kg, the_one_land_cover, the_one_biome,
     file = paste0(PATH_SAVE_UNCERTAINTY_PREC, "one_for_all.rda"))

