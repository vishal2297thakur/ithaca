# Partitions total evaporation to different classes and creates the bar plots 
# of climate types and dataset agreement

source('source/partition_evap.R')
source('source/graphics.R')
source('source/geo_functions_evap.R')

library(ggthemes)
library(scales)

## Data 
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
evap_grid <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_volume_grid.rds"))

## Variables
evap_mask[, KG_class_1_name := factor(KG_class_1_name, levels = levels(evap_mask$KG_class_1_name)[c(5, 4, 2, 3, 1)])]
levels(evap_mask$rel_dataset_agreement) <- c("High", "Above average", "Average", "Below average", "Low")

land_cover_class <- merge(evap_mask[, .(lat, lon, rel_dataset_agreement, land_cover_short_class, KG_class_1_name)], evap_grid[, .(lon, lat, evap_volume_year)], by = c("lon", "lat"))
biome_class <- merge(evap_mask[, .(lat, lon, rel_dataset_agreement, biome_short_class, KG_class_1_name)], evap_grid[, .(lon, lat, evap_volume_year)], by = c("lon", "lat"))
elevation_class <- merge(evap_mask[, .(lat, lon, rel_dataset_agreement, elev_class, KG_class_1_name)], evap_grid[, .(lon, lat, evap_volume_year)], by = c("lon", "lat"))
evap_quant <- merge(evap_mask[, .(lat, lon, rel_dataset_agreement, evap_quant, KG_class_1_name)], evap_grid[, .(lon, lat, evap_volume_year)], by = c("lon", "lat"))

## Analysis
### Land use
land_cover_evap <- land_cover_class[, .(evap_sum = sum(evap_volume_year)), .(KG_class_1_name, land_cover_short_class)]
land_cover_evap <- land_cover_evap[complete.cases(land_cover_evap)]
land_cover_evap <- land_cover_evap[order(KG_class_1_name, land_cover_short_class), ]

land_cover_agreement <- land_cover_class[, .(evap_sum = sum(evap_volume_year)), .(rel_dataset_agreement, land_cover_short_class)]
land_cover_agreement <- land_cover_agreement[complete.cases(land_cover_agreement)]
land_cover_agreement <- land_cover_agreement[order(rel_dataset_agreement, land_cover_short_class), ]
land_cover_agreement[, land_cover_sum := sum(evap_sum), land_cover_short_class]
land_cover_agreement[, land_cover_fraction := evap_sum / land_cover_sum]

### Biome types
biome_evap <- biome_class[, .(evap_sum = sum(evap_volume_year)), .(KG_class_1_name, biome_short_class)]
biome_evap <- biome_evap[complete.cases(biome_evap)]
biome_evap <- biome_evap[order(KG_class_1_name, biome_short_class), ]

biome_agreement <- biome_class[, .(evap_sum = sum(evap_volume_year)), .(rel_dataset_agreement, biome_short_class)]
biome_agreement <- biome_agreement[complete.cases(biome_agreement)]
biome_agreement <- biome_agreement[order(rel_dataset_agreement, biome_short_class), ]
biome_agreement[, biome_sum := sum(evap_sum), biome_short_class]
biome_agreement[, biome_fraction := evap_sum / biome_sum]

### Elevation
elevation_evap <- elevation_class[, .(evap_sum = sum(evap_volume_year)), .(KG_class_1_name, elev_class)]
elevation_evap <- elevation_evap[complete.cases(elevation_evap)]
elevation_evap <- elevation_evap[order(KG_class_1_name, elev_class), ]

elevation_agreement <- elevation_class[, .(evap_sum = sum(evap_volume_year)), .(rel_dataset_agreement, elev_class)]
elevation_agreement <- elevation_agreement[complete.cases(elevation_agreement)]
elevation_agreement <- elevation_agreement[order(rel_dataset_agreement, elev_class), ]
elevation_agreement[, elev_sum := sum(evap_sum), elev_class]
elevation_agreement[, elev_fraction := evap_sum / elev_sum]

### Evaporation quantiles
evap_quant_evap <- evap_quant[, .(evap_sum = sum(evap_volume_year)), .(KG_class_1_name, evap_quant)]
evap_quant_evap <- evap_quant_evap[complete.cases(evap_quant_evap)]
evap_quant_evap <- evap_quant_evap[order(KG_class_1_name, evap_quant), ]

evap_quant_agreement <- evap_quant[, .(evap_sum = sum(evap_volume_year)), .(rel_dataset_agreement, evap_quant)]
evap_quant_agreement <- evap_quant_agreement[complete.cases(evap_quant_agreement)]
evap_quant_agreement <- evap_quant_agreement[order(rel_dataset_agreement, evap_quant), ]
evap_quant_agreement[, evap_quant_sum := sum(evap_sum), evap_quant]
evap_quant_agreement[, evap_quant_fraction := evap_sum / evap_quant_sum]

## Save data
save(land_cover_evap, land_cover_agreement, biome_evap, biome_agreement, 
     elevation_evap, elevation_agreement, evap_quant_evap, evap_quant_agreement, 
     file = paste0(PATH_SAVE_PARTITION_EVAP, "partition_evap.Rdata"))
load(paste0(PATH_SAVE_PARTITION_EVAP, "partition_evap.Rdata"))

## Figures Main
### Land Use
fig_land_cover_partition_evap_volume <- ggplot(land_cover_evap[land_cover_short_class != "Other"]) +
  geom_bar(aes(x = reorder(land_cover_short_class,-(evap_sum)), y = evap_sum, fill = KG_class_1_name), stat = "identity") +
  scale_y_continuous(label = axis_scientific) +
  xlab('Land cover type')  +
  ylab(bquote('Evaporation sum ['~km^3~year^-1~']'))  +
  labs(fill = 'Climate type')  +
  scale_fill_manual(values = colset_KG_1_names) +
  theme_light() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

land_cover_agreement$land_cover_short_class <- factor(land_cover_agreement$land_cover_short_class, 
                                                          levels = c("Forests", "Savannas", "Croplands", "Shrublands", "Grasslands", "Water", "Barren", "Snow/Ice", "Other"))
fig_land_cover_partition_fraction <- ggplot(land_cover_agreement[land_cover_short_class != "Other"]) +
  geom_bar(aes(x = land_cover_short_class, y = land_cover_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('Land cover type')  +
  ylab('Fraction')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = colset_RdBu_5) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))
### Biomes
fig_biome_partition_evap_volume <- ggplot(biome_evap) +
  geom_bar(aes(x = reorder(biome_short_class, -(evap_sum)), y = evap_sum, fill = KG_class_1_name), stat = "identity") +
  scale_y_continuous(label = axis_scientific) +
  xlab('Biome')  +
  ylab(bquote('Evaporation sum ['~km^3~year^-1~']'))  +
  labs(fill = 'Climate type')  +
  scale_fill_manual(values = colset_KG_1_names) +
  theme_light() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

biome_agreement$biome_short_class <- factor(biome_agreement$biome_short_class, 
                                                  levels = c("T/S Forests", "T/S Grasslands", "T. Forests", 
                                                             "B. Forests", "T. Grasslands", "Deserts", "Tundra", 
                                                             "Flooded", "M. Grasslands", "Mediterranean", NA))
fig_biome_partition_fraction <- ggplot(biome_agreement) +
  geom_bar(aes(x = biome_short_class, y = biome_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('Biome')  +
  ylab('Fraction')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = colset_RdBu_5) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

### Elevation
fig_elevation_partition_evap_volume <- ggplot(elevation_evap) +
  geom_bar(aes(x = elev_class, y = evap_sum, fill = KG_class_1_name), stat = "identity") +
  scale_y_continuous(label = axis_scientific) +
  xlab('Elevation [m]')  +
  ylab(bquote('Evaporation sum ['~km^3~year^-1~']'))  +
  labs(fill = 'Climate type')  +
  scale_fill_manual(values = colset_KG_1_names) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

fig_elevation_partition_fraction <- ggplot(elevation_agreement) +
  geom_bar(aes(x = elev_class, y = elev_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('Elevation [m]')  +
  ylab('Fraction')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = colset_RdBu_5) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

### Evaporation classes
fig_evap_partition_evap_volume <- ggplot(evap_quant_evap) +
  geom_bar(aes(x = evap_quant, y = evap_sum, fill = KG_class_1_name), stat = "identity") +
  scale_y_continuous(label = axis_scientific) +
  xlab('Evaporation intensity class')  +
  ylab(bquote('Evaporation sum ['~km^3~year^-1~']'))  +
  labs(fill = 'Climate type')  +
  scale_fill_manual(values = colset_KG_1_names) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

fig_evap_partition_fraction <- ggplot(evap_quant_agreement) +
  geom_bar(aes(x = evap_quant, y = evap_quant_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('Evaporation intensity class')  +
  ylab('Fraction')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = colset_RdBu_5) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

### Figure 1
gg_fig_1 <- ggarrange(fig_land_cover_partition_evap_volume, fig_biome_partition_evap_volume, 
                      fig_elevation_partition_evap_volume, fig_evap_partition_evap_volume, 
                    labels = c('a', 'b', 'c', 'd'), align = 'hv',
                    common.legend = T, legend = 'right', 
                    nrow = 2, ncol = 2)
ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "partition_volume_climate.png"), width = 10, height = 10)

### Figure 2
gg_fig_2 <- ggarrange(fig_land_cover_partition_fraction,fig_biome_partition_fraction,
                      fig_elevation_partition_fraction, fig_evap_partition_fraction,
                    labels = c('a', 'b', 'c', 'd'), align = 'hv',
                    common.legend = T, legend = 'right', 
                    nrow = 2, ncol = 2)
ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "partition_fraction_agreement.png"), width = 10, height = 10)

