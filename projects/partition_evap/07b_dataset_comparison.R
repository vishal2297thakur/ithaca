# Look at min, max, mean, median of each dataset

source('source/partition_evap.R')
source('source/graphics.R')

## Data 
masks_global <- readRDS(paste0(PATH_SAVE, "/misc/masks_global.rds"))
evap_mean_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_datasets.rds"))

evap_mean_data_mask <- merge(evap_mean_datasets, masks_global, by = c("lon", "lat"))
  
evap_dataset_KG <-  evap_mean_data_mask[,.(min_evap = min(evap_mean), max = max(evap_mean), mean = mean(evap_mean), median = median(evap_mean)), .(dataset, KG_class_1_name)]
evap_dataset_land_cover <-  evap_mean_data_mask[,.(min_evap = min(evap_mean), max = max(evap_mean), mean = mean(evap_mean), median = median(evap_mean)), .(dataset, land_cover_short_class)]
evap_dataset_biomes <-  evap_mean_data_mask[,.(min_evap = min(evap_mean), max = max(evap_mean), mean = mean(evap_mean), median = median(evap_mean)), .(dataset, biome_short_class)]


  
ggplot(evap_mean_data_mask, aes(y = evap_mean)) +
  geom_boxplot(aes(y = evap_mean, x = dataset)) +
  facet_wrap(~KG_class_1_name, nrow = 1, scales = "free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  labs(y = "Mean annual evaporation rate [mm]", x = "Dataset")
  

ggplot(evap_mean_data_mask, aes(y = evap_mean)) +
  geom_boxplot(aes(y = evap_mean, x = dataset)) +
  facet_wrap(~land_cover_short_class, scales = "free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  labs(y = "Mean annual evaporation rate [mm]", x = "Dataset")


ggplot(evap_mean_data_mask, aes(y = evap_mean)) +
  geom_boxplot(aes(y = evap_mean, x = dataset)) +
  facet_wrap(~biome_short_class, scales = "free")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  labs(y = "Mean annual evaporation rate [mm]", x = "Dataset")
