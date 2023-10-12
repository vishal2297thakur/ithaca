# Calculate area weights per country by classes
source("source/uncertainty_prec.R")

## Data
masks <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC_SPATIAL,
                        "pRecipe_masks.rds"))

## Analysis
areas <- masks[, .(lon, lat, value = 1)] %>% rasterFromXYZ() %>% area() %>%
  as.data.frame(xy = TRUE, na.rm = TRUE)
setnames(areas, c("lon", "lat", "cell_area"))

### Elevation
prec_masks <- merge(masks, areas, by = c("lon", "lat"))

prec_masks[, country_area := sum(cell_area), .(country)
           ][, biome_weight := sum(cell_area)/country_area,
             .(country, biome_short_class)
             ][, elev_weight := sum(cell_area)/country_area,
               .(country, elev_class)
               ][, kg_weight := sum(cell_area)/country_area,
                 .(country, KG_class_1_name)
                 ][, land_cover_weight := sum(cell_area)/country_area,
                   .(country, land_cover_short_class)]

prec_masks <- unique(prec_masks[!is.na(country), .(country, biome_short_class,
                                                   biome_weight, elev_class,
                                                   elev_weight, KG_class_1_name,
                                                   kg_weight,
                                                   land_cover_short_class,
                                                   land_cover_weight)])

## Save
saveRDS(prec_masks,
        file = paste0(PATH_SAVE_UNCERTAINTY_PREC, "country_weights.rds"))
