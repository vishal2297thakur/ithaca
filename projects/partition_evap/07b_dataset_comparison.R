# Look at min, max, mean, median of each dataset

source('source/partition_evap.R')
source('source/graphics.R')

## Data 
masks_global <- readRDS(paste0(PATH_SAVE, "/misc/masks_global.rds"))
evap_mean_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_datasets.rds"))

evap_mean_data_mask <- merge(evap_mean_datasets, masks_global, by = c("lon", "lat"))
  
evap_dataset_KG <-  evap_mean_data_mask[,.(min_evap = min(evap_mean), max_evap = max(evap_mean), mean_evap = mean(evap_mean), median_evap = median(evap_mean), q25_evap = quantile(evap_mean, 0.25), q75_evap = quantile(evap_mean, 0.75)), .(dataset, KG_class_1_name)]
evap_dataset_land_cover <-  evap_mean_data_mask[,.(min_evap = min(evap_mean), max_evap = max(evap_mean), mean_evap = mean(evap_mean), median_evap = median(evap_mean), q25_evap = quantile(evap_mean, 0.25), q75_evap = quantile(evap_mean, 0.75)), .(dataset, land_cover_short_class)]
evap_dataset_biomes <-  evap_mean_data_mask[,.(min_evap = min(evap_mean), max_evap = max(evap_mean), mean_evap = mean(evap_mean), median_evap = median(evap_mean), q25_evap = quantile(evap_mean, 0.25), q75_evap = quantile(evap_mean, 0.75)), .(dataset, biome_short_class)]
evap_dataset_lat <-  evap_mean_data_mask[,.(min_evap = min(evap_mean), max_evap = max(evap_mean), mean_evap = mean(evap_mean), median_evap = median(evap_mean), q25_evap = quantile(evap_mean, 0.25), q75_evap = quantile(evap_mean, 0.75)), .(dataset, lat)]


ggplot(evap_dataset_KG, aes(x = dataset)) +
  geom_boxplot(
    aes(ymin = min_evap, 
        ymax = max_evap, 
        middle = median_evap, 
        lower = q25_evap, 
        upper = q75_evap),
    stat = "identity") +
  facet_wrap(~KG_class_1_name, nrow = 1, scales = "free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  labs(y = "Mean annual evaporation rate [mm]", x = "Dataset")


ggplot(evap_dataset_land_cover, aes(x = dataset)) +
  geom_boxplot(
    aes(ymin = min_evap, 
        ymax = max_evap, 
        middle = median_evap, 
        lower = q25_evap, 
        upper = q75_evap),
    stat = "identity") +
  facet_wrap(~land_cover_short_class, scales = "free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  labs(y = "Mean annual evaporation rate [mm]", x = "Dataset")

ggplot(evap_dataset_biomes, aes(x = dataset)) +
  geom_boxplot(
    aes(ymin = min_evap, 
        ymax = max_evap, 
        middle = median_evap, 
        lower = q25_evap, 
        upper = q75_evap),
    stat = "identity") +
  facet_wrap(~biome_short_class, scales = "free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  labs(y = "Mean annual evaporation rate [mm]", x = "Dataset")

ggplot(evap_dataset_lat, aes(x = dataset)) +
  geom_line(
    aes(x = lat, 
        y = median_evap, 
        col = dataset))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  labs(y = "Mean annual evaporation rate [mm]", x = "Lat", col = "Datset")

