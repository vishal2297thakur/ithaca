# Summary for EGU 2024

source('source/evap_trend.R')

# Changes in ET ----
## global changes in ET ----
evap_indices <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_slope_indices.rds"))

land_area <- evap_indices[,sum(area)]
uncertain_area <- evap_indices[trend == "uncertain", sum(area)]
no_trend_area <- evap_indices[trend == "no trend", sum(area)]
pos_trend_area <- evap_indices[grepl("positive", trend) == TRUE, sum(area)]
neg_trend_area <- evap_indices[grepl("negative", trend) == TRUE, sum(area)]

uncertain_area/land_area
no_trend_area/land_area
pos_trend_area/land_area
neg_trend_area/land_area

## biomes ----

biome_uncertainty <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "biomes_uncertainty.rds"))
pos_area <- biome_uncertainty[grepl("positive", trend) == TRUE, .(positive_area = sum(trend_area), biome_area), biome_short_class]
pos_area[, fraction := positive_area/biome_area]
