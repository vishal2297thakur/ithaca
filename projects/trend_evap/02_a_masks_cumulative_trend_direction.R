# Cumulative significant trend direction per mask item  ----
source('source/evap_trend.R')
source('source/graphics.R')
source('source/geo_functions.R')

## Data ----
### Input Data generated in projects/partition_evap/04
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
### Input Data generated in projects/evap_trend/01_a
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope.rds"))  

## Analysis ----
evap_trend[, trend_direction := factor(trend_direction, level = c("positive significant", "positive", "negative", "negative significant"), ordered = T),]
evap_trend_masks <- merge(evap_trend, evap_mask, all.x = T, by = c("lon", "lat"))
grid_cell_area <- unique(evap_trend[, .(lon, lat)]) %>% grid_area() # m2
evap_trend_masks <- grid_cell_area[evap_trend_masks, on = .(lon, lat)]

### Biome types ----
biome_trends <- evap_trend_masks[,.(trend_area = sum(area)),.(trend_direction, biome_class, dataset)]
biome_trends <- biome_trends[complete.cases(biome_trends)]
biome_trends[, biome_area:= sum(trend_area), .(biome_class, dataset)]
biome_trends[, biome_fraction:= trend_area/biome_area]
biome_trends[grepl("Tundra", biome_class) == TRUE, biome_short_class := "Tundra"]
biome_trends[grepl("Boreal Forests", biome_class) == TRUE, biome_short_class := "B. Forests"]
biome_trends[grepl("Dry Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Dry BL Forests"]
biome_trends[grepl("Moist Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Moist BL Forests"]
biome_trends[grepl("Subtropical Coniferous Forests", biome_class) == TRUE, biome_short_class := "T/S Coni. Forests"]
biome_trends[grepl("Temperate Conifer Forests", biome_class) == TRUE, biome_short_class := "T. Coni. Forests"]
biome_trends[grepl("Temperate Broadleaf & Mixed Forests", biome_class) == TRUE, biome_short_class := "T. BL Forests"]
biome_trends[grepl("Temperate Grasslands", biome_class) == TRUE, biome_short_class := "T. Grasslands"]
biome_trends[grepl("Subtropical Grasslands", biome_class) == TRUE, biome_short_class := "T/S Grasslands"]
biome_trends[grepl("Montane Grasslands", biome_class) == TRUE, biome_short_class := "M. Grasslands"]
biome_trends[grepl("Flooded", biome_class) == TRUE, biome_short_class := "Flooded"]
biome_trends[grepl("Mangroves", biome_class) == TRUE, biome_short_class := "Mangroves"]
biome_trends[grepl("Deserts", biome_class) == TRUE, biome_short_class := "Deserts"]
biome_trends[grepl("Mediterranean", biome_class) == TRUE, biome_short_class := "Mediterranean"]
biome_trends[grepl("N/A", biome_class) == TRUE, biome_short_class := NA]
biome_trends[, biome_short_class := factor(biome_short_class)]
biome_trends <- biome_trends[complete.cases(biome_trends)]

#### plot biome trend direction ----
ggplot(biome_trends) +
  geom_bar(aes(x = dataset, y = biome_fraction, fill = trend_direction), stat = "identity") +
  xlab('Dataset')  +
  ylab('Area fraction')  +
  labs(fill = 'Trend direction')  +
  scale_fill_manual(values = c("negative significant" = "darkblue", "positive significant" = "darkred", "negative" = "royalblue1", "positive" = "lightcoral"))+
  theme_light() +
  facet_wrap(~biome_short_class, ncol = 1)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "bar_biome_cumulative_fraction_evap_slope_direction_per_dataset.png"), 
       width = 8, height = 12)

### ipcc ref regions  ----
ipcc_trends <- evap_trend_masks[,.(trend_area = sum(area)),.(trend_direction, IPCC_ref_region, dataset)]
ipcc_trends <- ipcc_trends[complete.cases(ipcc_trends)]
ipcc_trends[, ipcc_area:= sum(trend_area), .(IPCC_ref_region, dataset)]
ipcc_trends[, ipcc_fraction:= trend_area/ipcc_area]


ggplot(ipcc_trends) +
  geom_bar(aes(x = dataset, y = ipcc_fraction, fill = trend_direction), stat = "identity") +
  xlab('Dataset')  +
  ylab('Fraction')  +
  labs(fill = 'Trend direction')  +
  scale_fill_manual(values = c("negative significant" = "darkblue", "positive significant" = "darkred", "negative" = "royalblue1", "positive" = "lightcoral"))+
  theme_light() +
  facet_wrap(~IPCC_ref_region, ncol = 3)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "bar_ipcc_cumulative_fraction_evap_slope_direction_per_dataset.png"), 
       width = 12, height = 16)

### land use ----
land_trends <- evap_trend_masks[,.(trend_area = sum(area)),.(trend_direction, land_cover_short_class, dataset)]
land_trends <- land_trends[complete.cases(land_cover_short_class)]
land_trends[, land_area:= sum(trend_area), .(land_cover_short_class, dataset)]
land_trends[, land_fraction:= trend_area/land_area]

ggplot(land_trends) +
  geom_bar(aes(x = dataset, y = land_fraction, fill = trend_direction), stat = "identity") +
  xlab('Dataset')  +
  ylab('Fraction')  +
  labs(fill = 'Trend direction')  +
  scale_fill_manual(values = c("negative significant" = "darkblue", "positive significant" = "darkred", "negative" = "royalblue1", "positive" = "lightcoral"))+
  theme_light() +
  facet_wrap(~land_cover_short_class, ncol = 1)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "bar_land_cover_cumulative_fraction_evap_slope_direction_per_dataset.png"), 
       width = 8, height = 12)


### elevation ----
elev_trends <- evap_trend_masks[,.(trend_area = sum(area)),.(trend_direction, elev_class, dataset)]
elev_trends <- elev_trends[complete.cases(elev_class)]
elev_trends[, elev_area:= sum(trend_area), .(elev_class, dataset)]
elev_trends[, elev_fraction:= trend_area/elev_area]

ggplot(elev_trends) +
  geom_bar(aes(x = dataset, y = elev_fraction, fill = trend_direction), stat = "identity") +
  xlab('Dataset')  +
  ylab('Fraction')  +
  labs(fill = 'Trend direction')  +
  scale_fill_manual(values = c("negative significant" = "darkblue", "positive significant" = "darkred", "negative" = "royalblue1", "positive" = "lightcoral"))+
  theme_light() +
  facet_wrap(~elev_class, ncol = 1)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "bar_elevation_cumulative_fraction_evap_slope_direction_per_dataset.png"), 
       width = 8, height = 12)


### KG 3 ----
KG_class_3_trends <- evap_trend_masks[,.(trend_area = sum(area)),.(trend_direction, KG_class_3, dataset)]
KG_class_3_trends <- KG_class_3_trends[complete.cases(KG_class_3_trends)]
KG_class_3_trends[, KG_class_3_area:= sum(trend_area), .(KG_class_3, dataset)]
KG_class_3_trends[, KG_class_3_fraction:= trend_area/KG_class_3_area]

ggplot(KG_class_3_trends) +
  geom_bar(aes(x = dataset, y = KG_class_3_fraction, fill = trend_direction), stat = "identity") +
  xlab('Dataset')  +
  ylab('Fraction')  +
  labs(fill = 'Trend direction')  +
  scale_fill_manual(values = c("negative significant" = "darkblue", "positive significant" = "darkred", "negative" = "royalblue1", "positive" = "lightcoral"))+
  theme_light() +
  facet_wrap(~KG_class_3, ncol = 3)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "bar_KG_3_cumulative_fraction_evap_slope_direction_per_dataset.png"), 
       width = 12, height = 16)

### Save data ----
saveRDS(biome_trends, paste0(PATH_SAVE_EVAP_TREND, "biome_cumulative_trend_direction_per_dataset.rds"))
saveRDS(ipcc_trends, paste0(PATH_SAVE_EVAP_TREND, "ipcc_cumulative_trend_direction_per_dataset.rds"))
saveRDS(land_trends, paste0(PATH_SAVE_EVAP_TREND, "land_cover_cumulative_trend_direction_per_dataset.rds"))
saveRDS(elev_trends, paste0(PATH_SAVE_EVAP_TREND, "elevation_cumulative_trend_direction_per_dataset.rds"))
saveRDS(KG_class_3_trends, paste0(PATH_SAVE_EVAP_TREND, "KG_3_cumulative_trend_direction_per_dataset.rds"))

