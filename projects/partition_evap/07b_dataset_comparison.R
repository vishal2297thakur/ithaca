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


evap_mean_data_mask_polar <- evap_mean_data_mask[KG_class_1_name == "Polar"]
evap_mean_data_mask_polar <- subset(evap_mean_data_mask_polar, select = c(lon, lat, evap_mean, dataset))
evap_mean_polar_wide <- dcast(evap_mean_data_mask_polar, formula = lon+lat~dataset, value.var = "evap_mean")
evap_mean_polar_camele<- melt(evap_mean_polar_wide, id.vars = c("camele", "lon", "lat"))


evap_mean_data_mask_barren<- evap_mean_data_mask[land_cover_short_class == "Barren"]
evap_mean_data_mask_barren <- subset(evap_mean_data_mask_barren, select = c(lon, lat, evap_mean, dataset))
evap_mean_barren_wide <- dcast(evap_mean_data_mask_barren, formula = lon+lat~dataset, value.var = "evap_mean")
evap_mean_barren_camele<- melt(evap_mean_barren_wide, id.vars = c("camele", "lon", "lat"))


ggplot(evap_mean_data_mask[land_cover_short_class == "Barren"], aes(x = dataset, y = evap_mean)) +
  geom_violin(fill = NA, lwd = 0.7, position = "identity") +
  geom_boxplot(width = .2, alpha = .7, show.legend = FALSE, col = "red") +
  geom_jitter(width = 0.1, alpha = .05, col = "gray") +
  facet_wrap(~dataset, scales = 'free') +
  theme_bw()


ggplot(evap_mean_barren_camele, aes(x = value, y = camele))+
  geom_point(col = "gray")+
  geom_abline(slope = 1, intercept = 0)+
  facet_wrap(.~variable, scales = "free")+
  theme_bw()

ggplot(evap_mean_barren_camele[value < 100 & camele < 100,], aes(x = value, y = camele))+
  geom_point(col = "gray")+
  geom_abline(slope = 1, intercept = 0)+
  facet_wrap(.~variable, scales = "free")+
  ggtitle(label = "Barren (E < 100 mm/y)")+
  theme_bw()

ggplot(evap_mean_barren_camele[value < 100,], aes(x = value, y = camele))+
  geom_point(col = "gray")+
  geom_abline(slope = 1, intercept = 0)+
  facet_wrap(.~variable, scales = "free")+
  ggtitle(label = "Barren (E < 100 mm/y)")+
  theme_bw()


ggplot(evap_mean_barren_camele[camele < 100,], aes(x = value, y = camele))+
  geom_point(col = "gray")+
  geom_bin2d() +
  geom_abline(slope = 1, intercept = 0)+
  facet_wrap(.~variable, scales = "free")+
  ggtitle(label = "Barren (E < 100 mm/y)")+
  theme_bw()

ggplot(evap_mean_polar_camele, aes(x = value, y = camele))+
  geom_point(col = "gray")+
  geom_density_2d()+
  geom_abline(slope = 1, intercept = 0)+
  facet_wrap(.~variable, scales = "free")+
  theme_bw()

ggplot(data = evap_mean_data_mask[evap_mean < 0,])+
  geom_tile(aes(x = lon, y = lat, fill = evap_mean))+
  facet_wrap(~dataset)+
  scale_fill_binned(type = "viridis", breaks = c(-Inf, seq(-100, 0, 20)))+
  theme_bw()

ggplot(data = evap_mean_data_mask[KG_class_1 == "E" ,])+
  geom_tile(aes(x = lon, y = lat, fill = evap_mean))+
  facet_wrap(~dataset)+
  scale_fill_binned(type = "viridis", breaks = c(seq(-250, 2500, 250)))+
  theme_bw()

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


