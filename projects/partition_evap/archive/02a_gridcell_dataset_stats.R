# Estimates the 2000-2019 mean and sd at each grid cell (mm) for each dataset (evap_mean_datasets.rds) 
# and the average annual volume (km3/yr) for all datasets per grid cell (evap_mean_volume_grid.rds)

source('source/partition_evap.R')
source('source/geo_functions.R')

## Data
evap_2000_2019 <- lapply(EVAP_FNAMES_2000_2019, brick)
names(evap_2000_2019) <- EVAP_FNAMES_SHORT_2000_2019 

## Analysis
registerDoParallel(cores = N_CORES - 1)
evap_datasets <- foreach(dataset_count = 1:n_datasets_2000_2019, .combine = rbind) %dopar% {
  dummie_brick <- evap_2000_2019[[dataset_count]]
  dummie_mean <- calc(dummie_brick, fun = mean, na.rm = TRUE) %>%
    as.data.frame(xy = TRUE, na.rm = TRUE) %>% as.data.table()
  dummie_sd <- calc(dummie_brick, fun = sd, na.rm = TRUE) %>%
    as.data.frame(xy = TRUE, na.rm = TRUE) %>% as.data.table()
  dummie_table <- merge(dummie_mean, dummie_sd, by = c('x', 'y'))
  setnames(dummie_table, c('lon', 'lat', 'evap_mean', 'evap_sd'))
  dummie_table$dataset <- names(evap_2000_2019[dataset_count])
  return(dummie_table)
}
setkeyv(evap_datasets, c("lon", "lat", "dataset"))
evap_datasets[, n_datasets := .N, .(lon, lat)]
saveRDS(evap_datasets, paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_datasets_complete.rds"))

evap_datasets <- evap_datasets[n_datasets >= MIN_N_DATASETS]
evap_datasets[dataset %in% EVAP_DATASETS_REANAL, dataset_type := 'reanalysis'
              ][dataset %in% EVAP_DATASETS_REMOTE, dataset_type := 'remote sensing'
                ][dataset %in% EVAP_DATASETS_HYDROL, dataset_type := 'hydrologic model'
                  ][dataset %in% EVAP_DATASETS_ENSEMB, dataset_type := 'ensemble']

### Evaporation volumes 
grid_cell_area <- unique(evap_datasets[, .(lon, lat)]) %>% grid_area() # m2
evap_mean_datasets <- evap_datasets[, .(evap_mean = mean(evap_mean)), .(lon, lat, n_datasets)]
evap_volume <- grid_cell_area[evap_mean_datasets, on = .(lon, lat)]
evap_volume[, evap_volume_year := area * M2_TO_KM2 * evap_mean * MM_TO_KM] # km3

evap_datasets <- evap_datasets[, .(lon, lat, dataset, dataset_type, evap_mean, evap_sd)]
evap_datasets[, evap_mean := round(evap_mean, 2)][, evap_sd := round(evap_sd, 2)]

## Save data 
saveRDS(evap_datasets, paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_datasets.rds"))
saveRDS(evap_volume, paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_volume_grid.rds"))

## Code review

brks <- c(0, 0.5, seq(250, 2500, 250))
nb <- length(brks)
cols <- c("white", rainbow(nb-1))
for(dataset_count in 1:n_datasets_2000_2019){
  plot(evap_2000_2019[[dataset_count]][[10]], main = names(evap_2000_2019[dataset_count]),
       colNA = "black",
       col = cols,
       breaks = brks) 
}


brks <- c(-1.3, 0, 0.5, 2500)
cols <- c("green","white", "orchid2")
for(dataset_count in 1:n_datasets_2000_2019){
  plot(evap_2000_2019[[dataset_count]][[10]], main = names(evap_2000_2019[dataset_count]),
       colNA = "black",
       col = cols,
       breaks = brks)  
}




evap_mean_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_datasets.rds"))

library(ggplot2)

ggplot(evap_mean_datasets[dataset == EVAP_GLOBAL_DATASETS[12]])+
  geom_tile(aes(x = lon, y = lat, fill = evap_mean))+
  scale_fill_binned(type = "viridis", breaks = c(-100,0, 0.5, seq(500, 2500, 500)), limits = c(-100, 2500))+
  ggtitle(label = EVAP_GLOBAL_DATASETS[12])+
  theme_bw()




