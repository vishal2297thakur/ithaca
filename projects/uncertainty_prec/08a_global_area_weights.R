# Calculate area weights per country by classes
source("source/uncertainty_prec.R")

## Data
masks <- pRecipe_masks()

## Analysis
areas <- masks[, .(lon, lat, value = 1)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>% area() %>%
  as.data.frame(xy = TRUE, na.rm = TRUE)
setnames(areas, c("lon", "lat", "cell_area"))

### Divide country by classes
prec_masks <- merge(masks, areas, by = c("lon", "lat"))
prec_masks <- prec_masks[!is.na(country) & country != ""]

prec_masks[, country_area := sum(cell_area), .(country)
           ][!is.na(biome_short_class) & biome_short_class != "",
             biome_weight := sum(cell_area)/country_area,
             .(country, biome_short_class)
             ][!is.na(elev_class) & elev_class != "",
               elev_weight := sum(cell_area)/country_area,
               .(country, elev_class)
               ][!is.na(KG_class) & KG_class != "",
                 kg_weight := sum(cell_area)/country_area,
                 .(country, KG_class)
                 ][!is.na(land_cover_short_class) & land_cover_short_class != "",
                   land_cover_weight := sum(cell_area)/country_area,
                   .(country, land_cover_short_class)]

prec_masks <- unique(prec_masks[!is.na(country), .(country, biome_short_class,
                                                   biome_weight, elev_class,
                                                   elev_weight, KG_class,
                                                   kg_weight,
                                                   land_cover_short_class,
                                                   land_cover_weight)])

## Save
saveRDS(prec_masks,
        file = paste0(PATH_SAVE_UNCERTAINTY_PREC, "country_weights.rds"))
