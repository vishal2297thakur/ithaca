# Best data set per country
source("source/uncertainty_prec.R")

## Data
prec_masks <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "country_weights.rds"))

prec_biome_month <- fread(paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,
                                 "biome_month.csv"))
prec_biome_years <- fread(paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,
                                 "biome_years.csv"))

prec_elev_month <- fread(paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,
                                "elev_month.csv"))
prec_elev_years <- fread(paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,
                                "elev_years.csv"))

prec_kg_month <- fread(paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,
                              "kg_class_month.csv"))
prec_kg_years <- fread(paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,
                              "kg_class_years.csv"))

prec_land_cover_month <- fread(paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,
                                      "land_cover_month.csv"))
prec_land_cover_years <- fread(paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,
                                      "land_cover_years.csv"))

## Analysis
### Biome
country_biome_month <- merge(unique(prec_masks[, .(country,
                                                   biome_short_class,
                                                   biome_weight)]),
                             prec_biome_month, by = "biome_short_class",
                             allow.cartesian = TRUE)

### Biomes
prec_biome[, median_prec := median(value, na.rm = TRUE),
           .(biome_short_class, date)]
prec_biome <- prec_biome[, .(rmse_prec = rmse(median_prec, value)),
                         .(biome_short_class, dataset)] %>% unique()
prec_one_biome <- prec_biome[ , .SD[which.min(rmse_prec)], biome_short_class]

### Countries
prec_country[, median_prec := median(value, na.rm = TRUE), .(country, date)]
prec_country <- prec_country[, .(rmse_prec = rmse(median_prec, value)),
                             .(country, dataset)] %>% unique()
prec_one_country <- prec_country[ , .SD[which.min(rmse_prec)], country]

### Elevation
prec_elev[, median_prec := median(value, na.rm = TRUE), .(elev_class, date)]
prec_elev <- prec_elev[, .(rmse_prec = rmse(median_prec, value)),
                       .(elev_class, dataset)] %>% unique()
prec_one_elev <- prec_elev[ , .SD[which.min(rmse_prec)], elev_class]

### KG
prec_kg[, median_prec := median(value, na.rm = TRUE), .(KG_class_1_name, date)]
prec_kg <- prec_kg[, .(rmse_prec = rmse(median_prec, value)),
                   .(KG_class_1_name, dataset)] %>% unique()
prec_one_kg <- prec_kg[ , .SD[which.min(rmse_prec)], KG_class_1_name]

### Land
prec_land[, median_prec := median(value, na.rm = TRUE),
          .(land_cover_short_class, date)]
prec_land <- prec_land[, .(rmse_prec = rmse(median_prec, value)),
                       .(land_cover_short_class, dataset)] %>% unique()
prec_one_land <- prec_land[ , .SD[which.min(rmse_prec)], land_cover_short_class]
