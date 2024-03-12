# One for all results

source("source/uncertainty_prec.R")

install.packages(setdiff("DescTools", rownames(installed.packages())))

library(DescTools, include.only = "Mode")
## Data
load(paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_one_classes_normal.rda"))

## Analysis
### Biome
prec_biome <- copy(the_one_biome) %>%
  .[, dataset := Mode(one_for_all), by = "biome_short_class"] %>%
  .[, prob := .N/10000, by = c("biome_short_class", "one_for_all")] %>% unique()

biome_table <- copy(prec_biome) %>%
  .[one_for_all == dataset, .(biome_short_class, dataset, prob)] %>%
  unique()

### Elevation
prec_elev <- copy(the_one_elev) %>%
  .[, dataset := Mode(one_for_all), by = "elev_class"] %>%
  .[, prob := .N/10000, by = c("elev_class", "one_for_all")] %>% unique()

elev_table <- copy(prec_elev) %>%
  .[one_for_all == dataset, .(elev_class, dataset, prob)] %>%
  unique()

### Koppen
prec_kg <- copy(the_one_kg) %>%
  .[, dataset := Mode(one_for_all), by = "KG_class_1_name"] %>%
  .[, prob := .N/10000, by = c("KG_class_1_name", "one_for_all")] %>% unique()

kg_table <- copy(prec_kg) %>%
  .[one_for_all == dataset, .(KG_class_1_name, dataset, prob)] %>%
  unique()

### Land
prec_land <- copy(the_one_land_cover) %>%
  .[, dataset := Mode(one_for_all), by = "land_cover_short_class"] %>%
  .[, prob := .N/10000, by = c("land_cover_short_class", "one_for_all")] %>% unique()

land_cover_table <- copy(prec_land) %>%
  .[one_for_all == dataset, .(land_cover_short_class, dataset, prob)] %>%
  unique()

## Save
fwrite(biome_table, paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES, "biomes.csv"))
fwrite(elev_table, paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES, "elevations.csv"))
fwrite(kg_table, paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES, "koppen_geiger.csv"))
fwrite(land_cover_table,
       paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES, "land_cover.csv"))
