# Division of data into landcover, biome, elevation, quantile classes and ipcc reference regions  ----
source('source/evap_trend.R')

## Data 
evap_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets.rds"))
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
evap_grid <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_volume_grid.rds"))
evap_dataset_means <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_datasets.rds"))
dataset_types <- unique(evap_dataset_means[, .(dataset, dataset_type)])


## Analysis  ----
evap_datasets_volume <- merge(evap_datasets[dataset %in% EVAP_GLOBAL_DATASETS, .(lon, lat, year, dataset, evap)], 
                              evap_grid[, .(lon, lat, area)], 
                              by = c("lon", "lat"), all = TRUE)
evap_datasets_volume[, evap_volume := area * M2_TO_KM2 * evap * MM_TO_KM]
evap_datasets_volume <- evap_datasets_volume[dataset_types, on = .(dataset)]
evap_datasets_volume[, dataset_type := factor(dataset_type, levels =  c( "reanalysis", "remote sensing","hydrologic model","ensemble"), 
                                              labels = c( "Reanalyses", "Remote sensing","Hydrologic model","Ensemble"))]

### Land cover class  ----
land_cover_class <- merge(evap_mask[, .(lat, lon, land_cover_short_class)], 
                          evap_datasets_volume[, .(lon, lat, year, evap_volume, area, dataset, dataset_type)], 
                          by = c("lon", "lat"))
land_cover_class_global <- land_cover_class[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                            .(dataset, dataset_type, land_cover_short_class, year)]
land_cover_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]
 
### Biomes ----
biome_class <- merge(evap_mask[, .(lat, lon, biome_class)], 
                     evap_datasets_volume[, .(lon, lat, year, evap_volume, area, dataset, dataset_type)], 
                     by = c("lon", "lat"))
biome_class_global <- biome_class[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                  .(dataset, dataset_type, biome_class, year)]
biome_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]
biome_class_global <- biome_class_global[complete.cases(biome_class_global)]

### Elevation ----
elev_class <- merge(evap_mask[, .(lat, lon, elev_class)], 
                    evap_datasets_volume[, .(lon, lat, year, evap_volume, area, dataset, dataset_type)], 
                    by = c("lon", "lat"))
elev_class_global <- elev_class[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                .(dataset, dataset_type, elev_class, year)]
elev_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]

### Quantiles ----
evap_class <- merge(evap_mask[, .(lat, lon, evap_quant)], 
                    evap_datasets_volume[, .(lon, lat, year, evap_volume, area, dataset, dataset_type)], 
                    by = c("lon", "lat"))
evap_class_global <- evap_class[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                .(dataset, dataset_type, evap_quant, year)]
evap_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]

### IPCC reference region ----
ipcc_class <- merge(evap_mask[, .(lat, lon, IPCC_ref_region)], 
                    evap_datasets_volume[, .(lon, lat, year, evap_volume, area, dataset, dataset_type)], 
                    by = c("lon", "lat"))
ipcc_class_global <- ipcc_class[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                .(dataset, dataset_type, IPCC_ref_region, year)]
ipcc_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]


### KG level 3 classes
KG_class_3_class <- merge(evap_mask[, .(lat, lon, KG_class_3)], 
                    evap_datasets_volume[, .(lon, lat, year, evap_volume, area, dataset, dataset_type)], 
                    by = c("lon", "lat"))
KG_class_3_class_global <- KG_class_3_class[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                .(dataset, dataset_type, KG_class_3, year)]
KG_class_3_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]


## Save data ----
saveRDS(land_cover_class_global, paste0(PATH_SAVE_EVAP_TREND, "land_cover_class_global.rds"))
saveRDS(biome_class_global, paste0(PATH_SAVE_EVAP_TREND, "biome_class_global.rds"))
saveRDS(elev_class_global, paste0(PATH_SAVE_EVAP_TREND, "elev_class_global.rds"))
saveRDS(evap_class_global, paste0(PATH_SAVE_EVAP_TREND, "evap_class_global.rds"))
saveRDS(ipcc_class_global, paste0(PATH_SAVE_EVAP_TREND, "ipcc_class_global.rds"))
saveRDS(KG_class_3_class_global, paste0(PATH_SAVE_EVAP_TREND, "KG_3_class_global.rds"))
