# Best data set per Country
source("source/uncertainty_prec.R")

## Data
prec_min <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_min.rds"))

prec_top10 <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_top10.rds"))

prec_ncep <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_ncep.rds"))

prec_gpm <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_gpm.rds"))

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

###
prec_min <- merge(prec_min, prec_masks, by = c("lon", "lat"))

prec_top10 <- merge(prec_top10, prec_masks, by = c("lon", "lat"))

prec_ncep <- merge(prec_ncep, prec_masks, by = c("lon", "lat"))

prec_gpm <- merge(prec_gpm, prec_masks, by = c("lon", "lat"))

###
prec_min[, area_class := sum(area), .(dataset, country)
][, area_weights := area/area_class
][, weighted_t := t_prec*area_weights]

prec_min <- prec_min[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                         .(dataset, country)]

prec_top10[, area_class := sum(area), .(dataset, country)
][, area_weights := area/area_class
][, weighted_t := t_prec*area_weights]

prec_top10 <- prec_top10[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                         .(dataset, country)]

prec_ncep[, area_class := sum(area), .(dataset, country)
][, area_weights := area/area_class
][, weighted_t := t_prec*area_weights]

prec_ncep <- prec_ncep[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                         .(dataset, country)]

prec_gpm[, area_class := sum(area), .(dataset, country)
][, area_weights := area/area_class
][, weighted_t := t_prec*area_weights]

prec_gpm <- prec_gpm[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                         .(dataset, country)]

## Save
fwrite(prec_min,
       file = paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,"country_min.csv"))

fwrite(prec_top10,
       file = paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,"country_top10.csv"))

fwrite(prec_ncep,
       file = paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,"country_ncep.csv"))

fwrite(prec_gpm,
       file = paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,"country_gpm.csv"))

# Best data set per IPCC Region

## Data
prec_min <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_min.rds"))

prec_top10 <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_top10.rds"))

prec_ncep <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_ncep.rds"))

prec_gpm <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_gpm.rds"))

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
prec_min <- merge(prec_min, prec_masks, by = c("lon", "lat"))

prec_top10 <- merge(prec_top10, prec_masks, by = c("lon", "lat"))

prec_ncep <- merge(prec_ncep, prec_masks, by = c("lon", "lat"))

prec_gpm <- merge(prec_gpm, prec_masks, by = c("lon", "lat"))

###
prec_min[, area_class := sum(area), .(dataset, ipcc_region)
][, area_weights := area/area_class
][, weighted_t := t_prec*area_weights]

prec_min <- prec_min[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                     .(dataset, ipcc_region)]

prec_top10[, area_class := sum(area), .(dataset, ipcc_region)
][, area_weights := area/area_class
][, weighted_t := t_prec*area_weights]

prec_top10 <- prec_top10[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                         .(dataset, ipcc_region)]

prec_ncep[, area_class := sum(area), .(dataset, ipcc_region)
][, area_weights := area/area_class
][, weighted_t := t_prec*area_weights]

prec_ncep <- prec_ncep[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                       .(dataset, ipcc_region)]

prec_gpm[, area_class := sum(area), .(dataset, ipcc_region)
][, area_weights := area/area_class
][, weighted_t := t_prec*area_weights]

prec_gpm <- prec_gpm[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                     .(dataset, ipcc_region)]

## Save
fwrite(prec_min,
       file = paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,"ipcc_min.csv"))

fwrite(prec_top10,
       file = paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,"ipcc_top10.csv"))

fwrite(prec_ncep,
       file = paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,"ipcc_ncep.csv"))

fwrite(prec_gpm,
       file = paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,"ipcc_gpm.csv"))

# Best data set per Major Basin

## Data
prec_min <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_min.rds"))

prec_top10 <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_top10.rds"))

prec_ncep <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_ncep.rds"))

prec_gpm <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_gpm.rds"))

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

###
prec_min <- merge(prec_min, prec_masks, by = c("lon", "lat"))

prec_top10 <- merge(prec_top10, prec_masks, by = c("lon", "lat"))

prec_ncep <- merge(prec_ncep, prec_masks, by = c("lon", "lat"))

prec_gpm <- merge(prec_gpm, prec_masks, by = c("lon", "lat"))

###
prec_min[, area_class := sum(area), .(dataset, basin_name)
][, area_weights := area/area_class
][, weighted_t := t_prec*area_weights]

prec_min <- prec_min[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                     .(dataset, basin_name)]

prec_top10[, area_class := sum(area), .(dataset, basin_name)
][, area_weights := area/area_class
][, weighted_t := t_prec*area_weights]

prec_top10 <- prec_top10[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                         .(dataset, basin_name)]

prec_ncep[, area_class := sum(area), .(dataset, basin_name)
][, area_weights := area/area_class
][, weighted_t := t_prec*area_weights]

prec_ncep <- prec_ncep[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                       .(dataset, basin_name)]

prec_gpm[, area_class := sum(area), .(dataset, basin_name)
][, area_weights := area/area_class
][, weighted_t := t_prec*area_weights]

prec_gpm <- prec_gpm[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                     .(dataset, basin_name)]

## Save
fwrite(prec_min,
       file = paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,"basin_min.csv"))

fwrite(prec_top10,
       file = paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,"basin_top10.csv"))

fwrite(prec_ncep,
       file = paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,"basin_ncep.csv"))

fwrite(prec_gpm,
       file = paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,"basin_gpm.csv"))

# Best data set per KG Class

## Data
prec_min <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_min.rds"))

prec_top10 <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_top10.rds"))

prec_ncep <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_ncep.rds"))

prec_gpm <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_gpm.rds"))

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

###
prec_min <- merge(prec_min, prec_masks, by = c("lon", "lat"))

prec_top10 <- merge(prec_top10, prec_masks, by = c("lon", "lat"))

prec_ncep <- merge(prec_ncep, prec_masks, by = c("lon", "lat"))

prec_gpm <- merge(prec_gpm, prec_masks, by = c("lon", "lat"))

###
prec_min[, area_class := sum(area), .(dataset, KG_class)
][, area_weights := area/area_class
][, weighted_t := t_prec*area_weights]

prec_min <- prec_min[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                     .(dataset, KG_class)]

prec_top10[, area_class := sum(area), .(dataset, KG_class)
][, area_weights := area/area_class
][, weighted_t := t_prec*area_weights]

prec_top10 <- prec_top10[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                         .(dataset, KG_class)]

prec_ncep[, area_class := sum(area), .(dataset, KG_class)
][, area_weights := area/area_class
][, weighted_t := t_prec*area_weights]

prec_ncep <- prec_ncep[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                       .(dataset, KG_class)]

prec_gpm[, area_class := sum(area), .(dataset, KG_class)
][, area_weights := area/area_class
][, weighted_t := t_prec*area_weights]

prec_gpm <- prec_gpm[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                     .(dataset, KG_class)]

## Save
fwrite(prec_min,
       file = paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,"KG_min.csv"))

fwrite(prec_top10,
       file = paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,"KG_top10.csv"))

fwrite(prec_ncep,
       file = paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,"KG_ncep.csv"))

fwrite(prec_gpm,
       file = paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,"KG_gpm.csv"))
