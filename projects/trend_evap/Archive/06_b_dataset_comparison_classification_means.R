source('source/evap_trend.R')
source('source/geo_functions.R')
source('source/graphics.R')

masks_global <- readRDS(paste0(PATH_SAVE, "/misc/masks_global_IPCC.rds"))

evap_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets.rds"))
evap_datasets[, year := as.numeric(as.character(year))]

### dataset type
evap_datasets[dataset %in% EVAP_DATASETS_REANAL, dataset_type := 'Reanalysis'
][dataset %in% EVAP_DATASETS_REMOTE, dataset_type := 'Remote sensing'
][dataset %in% EVAP_DATASETS_HYDROL, dataset_type := 'Hydrologic model'
][dataset %in% EVAP_DATASETS_ENSEMB, dataset_type := 'Ensemble']

### Area for volume
grid_cell_area <- unique(evap_datasets[, .(lon, lat)]) %>% grid_area() # m2
evap_datasets_volume <- grid_cell_area[evap_datasets, on = .(lon, lat)]

evap_datasets_volume[, evap_volume:=  area * M2_TO_KM2 * evap * MM_TO_KM]

### merge classification
evap_masks <- merge(evap_datasets_volume, masks_global, by = c("lon", "lat"), all = T)

### Biomes
biome_class_global <- evap_masks[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                 .(dataset, dataset_type, biome_class, year)]
biome_class_global <- biome_class_global[complete.cases(biome_class_global)]

biome_class_global[, evap_mm := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM, ]

biome_class_global[ year < 2010, time_period := "2000-2009", ]
biome_class_global[ year >= 2010, time_period := "2010-2019", ]

biome_class <- biome_class_global[, .(evap_mean = mean(evap_mm), evap_median = median(evap_mm), evap_sd = sd(evap_mm)), .(time_period, biome_class, dataset)]
biome_class_datasettype <- biome_class_global[, .(evap_mean = mean(evap_mm), evap_median = median(evap_mm), evap_sd = sd(evap_mm)), .(time_period, biome_class, dataset_type)]


### Landcover

land_cover_class_global <- evap_masks[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                 .(dataset, dataset_type, land_cover_class, year)]
land_cover_class_global <- land_cover_class_global[complete.cases(land_cover_class_global)]

land_cover_class_global[, evap_mm := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM, ]

land_cover_class_global[ year < 2010, time_period := "2000-2009", ]
land_cover_class_global[ year >= 2010, time_period := "2010-2019", ]

land_cover_class <- land_cover_class_global[, .(evap_mean = mean(evap_mm), evap_median = median(evap_mm), evap_sd = sd(evap_mm)), .(time_period, land_cover_class, dataset)]
land_cover_class_datasettype <- land_cover_class_global[, .(evap_mean = mean(evap_mm), evap_median = median(evap_mm), evap_sd = sd(evap_mm)), .(time_period, land_cover_class, dataset_type)]


### IPCC
IPCC_ref_region_global <- evap_masks[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                      .(dataset, dataset_type, IPCC_ref_region, year)]
IPCC_ref_region_global <- IPCC_ref_region_global[complete.cases(IPCC_ref_region_global)]

IPCC_ref_region_global[, evap_mm := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM, ]

IPCC_ref_region_global[ year < 2010, time_period := "2000-2009", ]
IPCC_ref_region_global[ year >= 2010, time_period := "2010-2019", ]

IPCC_ref_region <- IPCC_ref_region_global[, .(evap_mean = mean(evap_mm), evap_median = median(evap_mm), evap_sd = sd(evap_mm)), .(time_period, IPCC_ref_region, dataset)]
IPCC_ref_region_datasettype <- IPCC_ref_region_global[, .(evap_mean = mean(evap_mm), evap_median = median(evap_mm), evap_sd = sd(evap_mm)), .(time_period, IPCC_ref_region, dataset_type)]

### Elevation

elev_class_global <- evap_masks[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                     .(dataset, dataset_type, elev_class, year)]
elev_class_global <- elev_class_global[complete.cases(elev_class_global)]

elev_class_global[, evap_mm := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM, ]

elev_class_global[ year < 2010, time_period := "2000-2009", ]
elev_class_global[ year >= 2010, time_period := "2010-2019", ]

elev_class <- elev_class_global[, .(evap_mean = mean(evap_mm), evap_median = median(evap_mm), evap_sd = sd(evap_mm)), .(time_period, elev_class, dataset)]
elev_class_datasettype <- elev_class_global[, .(evap_mean = mean(evap_mm), evap_median = median(evap_mm), evap_sd = sd(evap_mm)), .(time_period, elev_class, dataset_type)]


### KG class 3
KG_class_3_global <- evap_masks[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                .(dataset, dataset_type, KG_class_3, year)]
KG_class_3_global <- KG_class_3_global[complete.cases(KG_class_3_global)]

KG_class_3_global[, evap_mm := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM, ]

KG_class_3_global[ year < 2010, time_period := "2000-2009", ]
KG_class_3_global[ year >= 2010, time_period := "2010-2019", ]

KG_class_3 <- KG_class_3_global[, .(evap_mean = mean(evap_mm), evap_median = median(evap_mm), evap_sd = sd(evap_mm)), .(time_period, KG_class_3, dataset)]
KG_class_3_datasettype <- KG_class_3_global[, .(evap_mean = mean(evap_mm), evap_median = median(evap_mm), evap_sd = sd(evap_mm)), .(time_period, KG_class_3, dataset_type)]


saveRDS(biome_class, paste0(PATH_SAVE_EVAP_TREND, "biome_class_dataset_decadal_stats.rds"))  
saveRDS(biome_class_datasettype, paste0(PATH_SAVE_EVAP_TREND, "biome_class_datasetype_decadal_stats.rds"))  
saveRDS(KG_class_3, paste0(PATH_SAVE_EVAP_TREND, "KG_class_3_dataset_decadal_stats.rds"))  
saveRDS(KG_class_3_datasettype, paste0(PATH_SAVE_EVAP_TREND, "KG_class_3_datasetype_decadal_stats.rds"))  
saveRDS(elev_class, paste0(PATH_SAVE_EVAP_TREND, "elev_class_dataset_decadal_stats.rds"))  
saveRDS(elev_class_datasettype, paste0(PATH_SAVE_EVAP_TREND, "elev_class_datasetype_decadal_stats.rds"))  
saveRDS(IPCC_ref_region, paste0(PATH_SAVE_EVAP_TREND, "IPCC_ref_region_dataset_decadal_stats.rds"))  
saveRDS(IPCC_ref_region_datasettype, paste0(PATH_SAVE_EVAP_TREND, "IPCC_ref_region_datasetype_decadal_stats.rds"))  
saveRDS(land_cover_class, paste0(PATH_SAVE_EVAP_TREND, "land_cover_class_dataset_decadal_stats.rds"))  
saveRDS(land_cover_class_datasettype, paste0(PATH_SAVE_EVAP_TREND, "land_cover_class_datasetype_decadal_stats.rds"))  
