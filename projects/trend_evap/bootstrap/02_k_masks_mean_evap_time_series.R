# Division of data into landcover, biome, elevation, quantile classes and ipcc reference regions  ----
source('source/evap_trend.R')

## Data 
evap_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets.rds"))
### Input Data generated in projects/partition_evap/04
PATH_SAVE_PARTITION_EVAP <- paste0(PATH_SAVE, "partition_evap/")
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
evap_grid <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_volume_grid.rds"))

## Analysis  ----
evap_datasets_volume <- merge(evap_datasets[dataset %in% EVAP_GLOBAL_DATASETS, .(lon, lat, year, dataset, evap)], 
                              evap_grid[, .(lon, lat, area)], 
                              by = c("lon", "lat"), all = TRUE)
evap_datasets_volume[, evap_volume := area * M2_TO_KM2 * evap * MM_TO_KM]
evap_datasets_volume[, dataset_count := .N, .(lat, lon, year)]
evap_datasets_volume <- evap_datasets_volume[dataset_count >= 14]

### Land cover class  ----
land_cover_class <- merge(evap_mask[, .(lat, lon, land_cover_short_class)], 
                          evap_datasets_volume[, .(lon, lat, year, evap_volume, area, dataset)], 
                          by = c("lon", "lat"))
land_cover_class_global <- land_cover_class[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                            .(dataset, land_cover_short_class, year)]
land_cover_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]

### Biomes ----
biome_class <- merge(evap_mask[, .(lat, lon, biome_class)], 
                     evap_datasets_volume[, .(lon, lat, year, evap_volume, area, dataset)], 
                     by = c("lon", "lat"))
biome_class_global <- biome_class[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                  .(dataset, biome_class, year)]
biome_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]
biome_class_global <- biome_class_global[complete.cases(biome_class_global)]

### Elevation ----
elev_class <- merge(evap_mask[, .(lat, lon, elev_class)], 
                    evap_datasets_volume[, .(lon, lat, year, evap_volume, area, dataset)], 
                    by = c("lon", "lat"))
elev_class_global <- elev_class[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                .(dataset, elev_class, year)]
elev_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]

### Quantiles ----
evap_class <- merge(evap_mask[, .(lat, lon, evap_quant)], 
                    evap_datasets_volume[, .(lon, lat, year, evap_volume, area, dataset)], 
                    by = c("lon", "lat"))
evap_class_global <- evap_class[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                .(dataset, evap_quant, year)]
evap_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]

### IPCC reference region ----
ipcc_class <- merge(evap_mask[, .(lat, lon, IPCC_ref_region)], 
                    evap_datasets_volume[, .(lon, lat, year, evap_volume, area, dataset)], 
                    by = c("lon", "lat"))
ipcc_class_global <- ipcc_class[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                .(dataset, IPCC_ref_region, year)]
ipcc_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]


### KG level 3 classes
KG_class_3_class <- merge(evap_mask[, .(lat, lon, KG_class_3)], 
                          evap_datasets_volume[, .(lon, lat, year, evap_volume, area, dataset)], 
                          by = c("lon", "lat"))
KG_class_3_class_global <- KG_class_3_class[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                            .(dataset, KG_class_3, year)]
KG_class_3_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]



### KG level 2 classes
KG_class_2_class <- merge(evap_mask[, .(lat, lon, KG_class_2)], 
                          evap_datasets_volume[, .(lon, lat, year, evap_volume, area, dataset)], 
                          by = c("lon", "lat"))
KG_class_2_class_global <- KG_class_2_class[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                            .(dataset, KG_class_2, year)]
KG_class_2_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]



### KG level 1 classes
KG_class_1_class <- merge(evap_mask[, .(lat, lon, KG_class_1)], 
                          evap_datasets_volume[, .(lon, lat, year, evap_volume, area, dataset)], 
                          by = c("lon", "lat"))
KG_class_1_class_global <- KG_class_1_class[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                            .(dataset, KG_class_1, year)]
KG_class_1_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]


## Save data ----
saveRDS(land_cover_class_global, paste0(PATH_SAVE_EVAP_TREND, "land_cover_class_mean.rds"))
saveRDS(biome_class_global, paste0(PATH_SAVE_EVAP_TREND, "biome_class_mean.rds"))
saveRDS(elev_class_global, paste0(PATH_SAVE_EVAP_TREND, "elev_class_mean.rds"))
saveRDS(evap_class_global, paste0(PATH_SAVE_EVAP_TREND, "evap_class_mean.rds"))
saveRDS(ipcc_class_global, paste0(PATH_SAVE_EVAP_TREND, "ipcc_class_mean.rds"))
saveRDS(KG_class_3_class_global, paste0(PATH_SAVE_EVAP_TREND, "KG_3_class_mean.rds"))
saveRDS(KG_class_2_class_global, paste0(PATH_SAVE_EVAP_TREND, "KG_2_class_mean.rds"))
saveRDS(KG_class_1_class_global, paste0(PATH_SAVE_EVAP_TREND, "KG_1_class_mean.rds"))
