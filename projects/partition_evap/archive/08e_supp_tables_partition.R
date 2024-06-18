# Partition evapipitation to different classes for SI Tables
source('source/partition_evap.R')

## Data 
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
land_cover_class <- merge(evap_mask[, .(lat, lon, rel_dataset_agreement, 
                                        land_cover_short_class, KG_class_1_name)], 
                          evap_grid[, .(lon, lat, evap_volume_year)], 
                          by = c("lon", "lat"),
                          all = TRUE)
biome_class <- merge(evap_mask[, .(lat, lon, rel_dataset_agreement, biome_short_class, KG_class_1_name)], evap_grid[, .(lon, lat, evap_volume_year)], by = c("lon", "lat"))
elevation_class <- merge(evap_mask[, .(lat, lon, rel_dataset_agreement, elev_class, KG_class_1_name)], evap_grid[, .(lon, lat, evap_volume_year)], by = c("lon", "lat"))
evap_quant <- merge(evap_mask[, .(lat, lon, rel_dataset_agreement, evap_quant, KG_class_1_name)], evap_grid[, .(lon, lat, evap_volume_year)], by = c("lon", "lat"))

## Analysis
land_cover <- land_cover_class[, .(evap_sum = round(sum(evap_volume_year), 0)), .(land_cover_short_class)]
land_cover <- land_cover[complete.cases(land_cover)]
land_cover <- land_cover[order(-evap_sum)]
land_cover[, evap_cum_sum := cumsum(evap_sum)]
land_cover[, evap_cum_fraction := round(evap_cum_sum / sum(evap_sum), 2)]

biome <- biome_class[, .(evap_sum = round(sum(evap_volume_year), 0)), .(biome_short_class)]
biome <- biome[complete.cases(biome)]
biome <- biome[order(-evap_sum)]
biome[, evap_cum_sum := cumsum(evap_sum)]
biome[, evap_cum_fraction := round(evap_cum_sum / sum(evap_sum), 2)]

elevation <- elevation_class[, .(evap_sum = round(sum(evap_volume_year), 0)), .(elev_class)]
elevation <- elevation[complete.cases(elev_class)]
elevation <- elevation[order(-evap_sum)]
elevation[, evap_cum_sum := cumsum(evap_sum)]
elevation[, evap_cum_fraction := round(evap_cum_sum / sum(evap_sum), 2)]

evap_quant <- evap_quant[, .(evap_sum = round(sum(evap_volume_year), 0)), .(evap_quant)]
evap_quant <- evap_quant[complete.cases(evap_quant)]
evap_quant <- evap_quant[order(-evap_sum)]
evap_quant[, evap_cum_sum := cumsum(evap_sum)]
evap_quant[, evap_cum_fraction := round(evap_cum_sum / sum(evap_sum), 2)]

## Save data
write.csv(land_cover, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "partition_land_cover.csv"))
write.csv(biome, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "partition_biomes.csv"))
