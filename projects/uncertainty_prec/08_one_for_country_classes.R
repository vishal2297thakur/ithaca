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
country_biome_month <- merge(unique(prec_masks[, .(country, biome_short_class,
                                                   biome_weight)]),
                             prec_biome_month, by = "biome_short_class",
                             allow.cartesian = TRUE)
country_biome_month[, weighted_t := prec_t*biome_weight]
country_biome_month <- country_biome_month[, .(prec_t = sum(weighted_t,
                                                            na.rm = TRUE)),
                                           .(dataset, country)]

country_biome_years <- merge(unique(prec_masks[, .(country, biome_short_class,
                                                   biome_weight)]),
                             prec_biome_years, by = "biome_short_class",
                             allow.cartesian = TRUE)
country_biome_years[, weighted_t := prec_t*biome_weight]
country_biome_years <- country_biome_years[, .(prec_t = sum(weighted_t,
                                                            na.rm = TRUE)),
                                           .(dataset, country)]

### Elevation
country_elev_month <- merge(unique(prec_masks[, .(country, elev_class,
                                                  elev_weight)]),
                            prec_elev_month, by = "elev_class",
                            allow.cartesian = TRUE)
country_elev_month[, weighted_t := prec_t*elev_weight]
country_elev_month <- country_elev_month[, .(prec_t = sum(weighted_t,
                                                            na.rm = TRUE)),
                                           .(dataset, country)]

country_elev_years <- merge(unique(prec_masks[, .(country, elev_class,
                                                   elev_weight)]),
                            prec_elev_years, by = "elev_class",
                            allow.cartesian = TRUE)
country_elev_years[, weighted_t := prec_t*elev_weight]
country_elev_years <- country_elev_years[, .(prec_t = sum(weighted_t,
                                                            na.rm = TRUE)),
                                           .(dataset, country)]

### KG
country_kg_month <- merge(unique(prec_masks[, .(country, KG_class_1_name,
                                                  kg_weight)]),
                          prec_kg_month, by = "KG_class_1_name",
                          allow.cartesian = TRUE)
country_kg_month[, weighted_t := prec_t*kg_weight]
country_kg_month <- country_kg_month[, .(prec_t = sum(weighted_t,
                                                          na.rm = TRUE)),
                                         .(dataset, country)]

country_kg_years <- merge(unique(prec_masks[, .(country, KG_class_1_name,
                                                kg_weight)]),
                          prec_kg_years, by = "KG_class_1_name",
                          allow.cartesian = TRUE)
country_kg_years[, weighted_t := prec_t*kg_weight]
country_kg_years <- country_kg_years[, .(prec_t = sum(weighted_t,
                                                          na.rm = TRUE)),
                                         .(dataset, country)]

### Land
country_land_cover_month <- merge(unique(prec_masks[, .(country,
                                                        land_cover_short_class,
                                                        land_cover_weight)]),
                                  prec_land_cover_month,
                                  by = "land_cover_short_class",
                                  allow.cartesian = TRUE)
country_land_cover_month[, weighted_t := prec_t*land_cover_weight]
country_land_cover_month <- country_land_cover_month[,
                                                     .(prec_t = sum(weighted_t,
                                                                    na.rm = TRUE)),
                                                     .(dataset, country)]

country_land_cover_years <- merge(unique(prec_masks[, .(country,
                                                        land_cover_short_class,
                                                        land_cover_weight)]),
                                  prec_land_cover_years,
                                  by = "land_cover_short_class",
                                  allow.cartesian = TRUE)
country_land_cover_years[, weighted_t := prec_t*land_cover_weight]
country_land_cover_years <- country_land_cover_years[,
                                                     .(prec_t = sum(weighted_t,
                                                                    na.rm = TRUE)),
                                                     .(dataset, country)]

save(country_biome_month, country_elev_month, country_kg_month,
     country_land_cover_month, file = paste0(PATH_SAVE_UNCERTAINTY_PREC,
                                             "prec_country_class_month.rda"))

save(country_biome_years, country_elev_years, country_kg_years,
     country_land_cover_years, file = paste0(PATH_SAVE_UNCERTAINTY_PREC,
                                             "prec_country_class_years.rda"))

