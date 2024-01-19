# Best data set per Country
source("source/uncertainty_prec.R")

## Data
prec_month <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_unbiased.rds"))

prec_masks <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC_SPATIAL,
                             "pRecipe_masks.rds"))

## Analysis
### Prepare mask
prec_masks <- prec_masks[, .(lon, lat, country)]
prec_masks <- prec_masks[complete.cases(prec_masks)]

lonlat_area <- unique(prec_masks[, .(lon, lat)]) %>% .[, val := 1] %>%
  rasterFromXYZ(res = c(0.25, 0.25)) %>% area() %>% tabular() %>%
  .[, .(lon, lat, area = value)]

prec_masks <- merge(prec_masks, lonlat_area, by = c("lon", "lat"))

prec_month <- merge(prec_month, prec_masks, by = c("lon", "lat"))

###
prec_month[, area_class := sum(area), .(dataset, country)
][, area_weights := area/area_class
][, weighted_t := t_prec*area_weights]

prec_month <- prec_month[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                         .(dataset, country)]

## Save
fwrite(prec_month,
       file = paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES, "country_unbiased.csv"))

# Best data set per IPCC Region
## Data
prec_month <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_unbiased.rds"))

prec_masks <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC_SPATIAL,
                             "pRecipe_masks.rds"))

## Analysis
### Prepare mask
prec_masks <- prec_masks[, .(lon, lat, ipcc_region)]
prec_masks <- prec_masks[complete.cases(prec_masks)]

lonlat_area <- unique(prec_masks[, .(lon, lat)]) %>% .[, val := 1] %>%
  rasterFromXYZ(res = c(0.25, 0.25)) %>% area() %>% tabular() %>%
  .[, .(lon, lat, area = value)]

prec_masks <- merge(prec_masks, lonlat_area, by = c("lon", "lat"))

###
prec_month <- merge(prec_month, prec_masks, by = c("lon", "lat"))

prec_month[, area_class := sum(area), .(dataset, ipcc_region)
][, area_weights := area/area_class
][, weighted_t := t_prec*area_weights]

prec_month <- prec_month[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                     .(dataset, ipcc_region)]

## Save
fwrite(prec_month,
       file = paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES, "ipcc_unbiased.csv"))

# Best data set per Major Basin
## Data
prec_month <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_unbiased.rds"))

prec_masks <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC_SPATIAL,
                             "pRecipe_masks.rds"))

## Analysis
### Prepare mask
prec_masks <- prec_masks[, .(lon, lat, basin_name)]
prec_masks <- prec_masks[complete.cases(prec_masks)]

lonlat_area <- unique(prec_masks[, .(lon, lat)]) %>% .[, val := 1] %>%
  rasterFromXYZ(res = c(0.25, 0.25)) %>% area() %>% tabular() %>%
  .[, .(lon, lat, area = value)]

prec_masks <- merge(prec_masks, lonlat_area, by = c("lon", "lat"))

prec_month <- merge(prec_month, prec_masks, by = c("lon", "lat"))

###
prec_month[, area_class := sum(area), .(dataset, basin_name)
][, area_weights := area/area_class
][, weighted_t := t_prec*area_weights]

prec_month <- prec_month[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                     .(dataset, basin_name)]

## Save
fwrite(prec_month,
       file = paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES, "basin_unbiased.csv"))

# Best data set per KG Class
## Data
prec_month <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_unbiased.rds"))

prec_masks <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC_SPATIAL,
                             "pRecipe_masks.rds"))

## Analysis
### Prepare mask
prec_masks <- prec_masks[, .(lon, lat, KG_class)]
prec_masks <- prec_masks[complete.cases(prec_masks)]

lonlat_area <- unique(prec_masks[, .(lon, lat)]) %>% .[, val := 1] %>%
  rasterFromXYZ(res = c(0.25, 0.25)) %>% area() %>% tabular() %>%
  .[, .(lon, lat, area = value)]

prec_masks <- merge(prec_masks, lonlat_area, by = c("lon", "lat"))

prec_month <- merge(prec_month, prec_masks, by = c("lon", "lat"))

###
prec_month[, area_class := sum(area), .(dataset, KG_class)
][, area_weights := area/area_class
][, weighted_t := t_prec*area_weights]

prec_month <- prec_month[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                     .(dataset, KG_class)]

## Save
fwrite(prec_month,
       file = paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES, "kg_unbiased.csv"))

# Best data set per Biome
## Data
prec_month <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_unbiased.rds"))

prec_masks <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC_SPATIAL,
                             "pRecipe_masks.rds"))

## Analysis
### Prepare mask
prec_masks <- prec_masks[, .(lon, lat, biome_class, biome_short_class)]

lonlat_area <- unique(prec_masks[, .(lon, lat)]) %>% .[, val := 1] %>%
  rasterFromXYZ(res = c(0.25, 0.25)) %>% area() %>% tabular() %>%
  .[, .(lon, lat, area = value)]

prec_masks <- merge(prec_masks, lonlat_area, by = c("lon", "lat"))

prec_month <- merge(prec_month, prec_masks, by = c("lon", "lat"))

###
prec_month[, area_biome := sum(area), .(dataset, biome_short_class)
][, area_weights := area/area_biome
][, weighted_t := t_prec*area_weights]

prec_month <- prec_month[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                         .(dataset, biome_short_class)]

## Save
fwrite(prec_month,
       file = paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES, "biome_unbiased.csv"))

# Best data set per Elevation
## Data
prec_month <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_unbiased.rds"))

prec_masks <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC_SPATIAL,
                             "pRecipe_masks.rds"))

## Analysis
### Prepare mask
prec_masks <- prec_masks[, .(lon, lat, elev_class)]

lonlat_area <- unique(prec_masks[, .(lon, lat)]) %>% .[, val := 1] %>%
  rasterFromXYZ(res = c(0.25, 0.25)) %>% area() %>% tabular() %>%
  .[, .(lon, lat, area = value)]

prec_masks <- merge(prec_masks, lonlat_area, by = c("lon", "lat"))

prec_month <- merge(prec_month, prec_masks, by = c("lon", "lat"))

###
prec_month[, area_class := sum(area), .(dataset, elev_class)
][, area_weights := area/area_class
][, weighted_t := t_prec*area_weights]

prec_month <- prec_month[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                         .(dataset, elev_class)]

## Save
fwrite(prec_month,
       file = paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES, "elev_unbiased.csv"))

# Best data set per Land Cover
## Data
prec_month <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_unbiased.rds"))

prec_masks <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC_SPATIAL,
                             "pRecipe_masks.rds"))

## Analysis
### Prepare mask
prec_masks <- prec_masks[, .(lon, lat, land_cover_short_class)]

lonlat_area <- unique(prec_masks[, .(lon, lat)]) %>% .[, val := 1] %>%
  rasterFromXYZ(res = c(0.25, 0.25)) %>% area() %>% tabular() %>%
  .[, .(lon, lat, area = value)]

prec_masks <- merge(prec_masks, lonlat_area, by = c("lon", "lat"))

prec_month <- merge(prec_month, prec_masks, by = c("lon", "lat"))

###
prec_month[, area_class := sum(area), .(dataset, land_cover_short_class)
][, area_weights := area/area_class
][, weighted_t := t_prec*area_weights]

prec_month <- prec_month[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                         .(dataset, land_cover_short_class)]

## Save
fwrite(prec_month,
       file = paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,
                     "land_cover_unbiased.csv"))
