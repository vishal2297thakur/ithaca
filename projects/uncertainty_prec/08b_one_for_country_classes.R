# Best data set per country
source("source/uncertainty_prec.R")

## Data
prec_masks <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "country_weights.rds"))

prec_biome <- fread(paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,
                           "biome_ranking.csv"))

prec_elev <- fread(paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,
                          "elev_ranking.csv"))

prec_kg <- fread(paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,
                        "kg_ranking.csv"))

prec_land_cover <- fread(paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,
                                "land_cover_ranking.csv"))

## Analysis
### Biome
country_biome <- merge(unique(prec_masks[, .(country, biome_short_class,
                                             biome_weight)]),
                       prec_biome, by = "biome_short_class",
                       allow.cartesian = TRUE)
country_biome[, weighted_t := prec_t*biome_weight]
country_biome <- country_biome[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                               .(dataset, country)]

### Elevation
country_elev <- merge(unique(prec_masks[, .(country, elev_class, elev_weight)]),
                      prec_elev, by = "elev_class",
                      allow.cartesian = TRUE)
country_elev[, weighted_t := prec_t*elev_weight]
country_elev <- country_elev[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                             .(dataset, country)]

### KG
country_kg <- merge(unique(prec_masks[, .(country, KG_class, kg_weight)]),
                    prec_kg, by = "KG_class", allow.cartesian = TRUE)
country_kg[, weighted_t := prec_t*kg_weight]
country_kg <- country_kg[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                         .(dataset, country)]

### Land Cover
country_land_cover <- merge(unique(prec_masks[, .(country,
                                                  land_cover_short_class,
                                                  land_cover_weight)]),
                            prec_land_cover, by = "land_cover_short_class",
                            allow.cartesian = TRUE)
country_land_cover[, weighted_t := prec_t*land_cover_weight]
country_land_cover <- country_land_cover[, .(prec_t = sum(weighted_t,
                                                          na.rm = TRUE)),
                                         .(dataset, country)]

## Save
save(country_biome, country_elev, country_kg,
     country_land_cover, file = paste0(PATH_SAVE_UNCERTAINTY_PREC,
                                             "prec_country_class.rda"))
