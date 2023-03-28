# Estimation of ensemble statistics
source('source/partition_prec.R')
source('source/geo_functions.R')

## Data
prec_2000_2019 <- lapply(PREC_FNAMES_2000_2019, brick)
names(prec_2000_2019) <- PREC_FNAMES_SHORT_2000_2019 

## Analysis
registerDoParallel(cores = N_CORES - 1)
prec_mean <- foreach(dataset_count = 1:n_datasets_2000_2019) %dopar% {
  dummie <- prec_2000_2019[[dataset_count]]
  dummie <- calc(dummie, fun = mean, na.rm = TRUE)
  dummie
}
names(prec_mean) <- PREC_FNAMES_SHORT_2000_2019

prec_sd <- foreach(dataset_count = 1:n_datasets_2000_2019) %dopar% {
  dummie <- prec_2000_2019[[dataset_count]]
  dummie <- calc(dummie, fun = sd, na.rm = TRUE)
  dummie
}
names(prec_sd) <- PREC_FNAMES_SHORT_2000_2019

prec_mean_datasets <- foreach(dataset_count = 1:n_datasets_2000_2019, .combine = rbind) %dopar% {
  dummy <- as.data.table(as.data.frame(prec_mean[[dataset_count]], xy = TRUE, na.rm = TRUE))
  colnames(dummy) <- c('lon', 'lat', 'prec_mean')
  dummy$dataset <- names(prec_mean[dataset_count])
  dummy[, lon := round(lon, 3)]
  dummy[, lat := round(lat, 3)]
  dummy
}

prec_sd_datasets <- foreach(dataset_count = 1:n_datasets_2000_2019, .combine = rbind) %dopar% {
  dummy <- as.data.table(as.data.frame(prec_sd[[dataset_count]], xy = TRUE, na.rm = TRUE))
  colnames(dummy) <- c('lon', 'lat', 'prec_sd')
  dummy$dataset <- names(prec_sd[dataset_count])
  dummy[, lon := round(lon, 3)]
  dummy[, lat := round(lat, 3)]
  dummy
}

prec_mean_datasets[, n_datasets := .N, .(lon, lat)]
prec_mean_datasets <- prec_mean_datasets[n_datasets >= MIN_N_DATASETS]

prec_mean_datasets[dataset %in% PREC_DATASETS_OBS, dataset_type := 'ground stations']
prec_mean_datasets[dataset %in% PREC_DATASETS_REANAL, dataset_type := 'reanalysis']
prec_mean_datasets[dataset %in% PREC_DATASETS_REMOTE, dataset_type := 'remote sensing']
prec_mean_datasets[, prec_mean := round(prec_mean, 2)]

prec_datasets <- merge(prec_mean_datasets, prec_sd_datasets, by = c("lon", "lat", "dataset"))
prec_datasets <- prec_datasets[, .(lon, lat, dataset, dataset_type, prec_mean, prec_sd = round(prec_sd, 2))]

### Precipitation volumes 
grid_cell_area <- prec_datasets[, .(lon, lat)] %>% grid_area() # m2
prec_volume <- grid_cell_area[prec_datasets[, .(prec_mean = mean(prec_mean)), .(lon, lat)], on = .(lon, lat)]
prec_volume[, prec_volume_year := area * M2_TO_KM2 * prec_mean * MM_TO_KM][, prec_mean := NULL] # km3

## Save data 
saveRDS(prec_datasets, paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_datasets.rds"))
saveRDS(prec_volume, paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_volume_grid.rds"))
