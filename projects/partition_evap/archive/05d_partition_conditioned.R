# Same as 05b but for conditioned dataset agrrement

source('source/partition_evap.R')
source('source/graphics.R')
source('source/geo_functions.R')

library(ggthemes)
library(scales)

## Data 
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
evap_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_datasets.rds"))
evap_grid <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_volume_grid.rds"))

## Variables
evap_mask[, KG_class_1_name := relevel(factor(KG_class_1_name), "Polar")]
levels(evap_mask$evap_quant_dataset_agreement ) <- c("High", "Above average", "Average", "Below average", "Low")

climate_KG <- merge(evap_mask[, .(lat, lon, KG_class_1_name)], evap_grid[, .(lon, lat, area)], by = c("lon", "lat"))
datasets_KG <- merge(climate_KG, evap_datasets, by = c("lon", "lat"))
datasets_KG[, evap_volume_year := 12 * area * 10 ^ (-9) * evap_mean * 0.001][, evap_mean := NULL] # km3

land_cover_class <- merge(evap_mask[, .(lat, lon, evap_quant_dataset_agreement , land_cover_short_class, KG_class_1_name)], evap_grid[, .(lon, lat, evap_volume_year)], by = c("lon", "lat"))
biome_class <- merge(evap_mask[, .(lat, lon, evap_quant_dataset_agreement , biome_class, KG_class_1_name)], evap_grid[, .(lon, lat, evap_volume_year)], by = c("lon", "lat"))
elevation_class <- merge(evap_mask[, .(lat, lon, evap_quant_dataset_agreement , elev_class, KG_class_1_name)], evap_grid[, .(lon, lat, evap_volume_year)], by = c("lon", "lat"))
evap_quant <- merge(evap_mask[, .(lat, lon, evap_quant_dataset_agreement , evap_quant, KG_class_1_name)], evap_grid[, .(lon, lat, evap_volume_year)], by = c("lon", "lat"))

## Analysis
### Climate
datasets_KG[, .(area = round(sum(area), 2)), .(dataset, dataset_type)] #Antarctica 13.66 million km2
dataset_partition_KG <- datasets_KG[, .(evap_sum = round(sum(evap_volume_year), 0)), .(KG_class_1_name, dataset, dataset_type)]


#### Mean
partition_KG_global <- dcast(dataset_partition_KG, . ~ KG_class_1_name, 
                                     fun = mean, na.rm = TRUE)
colnames(partition_KG_global)[1] <- "Source"
partition_KG_dataset_types <- dcast(dataset_partition_KG, dataset_type ~ KG_class_1_name, 
                                           fun = mean, na.rm = TRUE)
colnames(partition_KG_dataset_types)[1] <- "Source"
partition_KG <- rbind(partition_KG_global, partition_KG_dataset_types)
partition_KG$Sum <- apply(partition_KG[, 2:6], 1, sum)
partition_KG[, Source := c("Global","Ensemble","Hydrologic model","Reanalysis","Remote Sensing")]

partition_KG_datasets <- dcast(dataset_partition_KG, dataset ~ KG_class_1_name, fun = mean, na.rm = TRUE)
partition_KG_datasets <- merge(evap_datasets[, .(dataset = unique(dataset)), dataset_type], partition_KG_datasets, by = 'dataset')
colnames(partition_KG_datasets)[1] <- c("Dataset")
partition_KG_datasets[, Sum := rowSums(.SD), .SDcols = 3:7]
partition_KG <- cbind(partition_KG[, 1], apply(partition_KG[, 2:7], 2, round, 0))

#### St. Dev
partition_KG_global_sd <- dcast(dataset_partition_KG, . ~ KG_class_1_name, 
                                     fun = sd, na.rm = TRUE)
colnames(partition_KG_global_sd)[1] <- "Source"
partition_KG_dataset_types_sd <- dcast(dataset_partition_KG, dataset_type ~ KG_class_1_name, 
                                           fun = sd, na.rm = TRUE)
colnames(partition_KG_dataset_types_sd)[1] <- "Source"
partition_KG_sd <- rbind(partition_KG_global_sd, partition_KG_dataset_types_sd)
partition_KG_sd[, Source := c("Global","Ensemble","Hydrologic model","Reanalysis","Remote Sensing")]
partition_KG_sd$Sum <- partition_KG_datasets[, sd(Sum, na.rm = TRUE)]
partition_KG_sd$Sum[2:3] <- partition_KG_datasets[, sd(Sum, na.rm = TRUE), dataset_type]$V1[c(1,2)]
partition_KG_sd <- cbind(partition_KG_sd[, 1], apply(partition_KG_sd[, 2:7], 2, round, 0))

### Land use
land_cover_evap <- land_cover_class[, .(evap_sum = sum(evap_volume_year)), .(KG_class_1_name, land_cover_short_class)]
land_cover_evap <- land_cover_evap[complete.cases(land_cover_evap)]
land_cover_evap <- land_cover_evap[order(KG_class_1_name, land_cover_short_class), ]

land_cover_agreement <- land_cover_class[, .(evap_sum = sum(evap_volume_year)), .(evap_quant_dataset_agreement , land_cover_short_class)]
land_cover_agreement <- land_cover_agreement[complete.cases(land_cover_agreement)]
land_cover_agreement <- land_cover_agreement[order(evap_quant_dataset_agreement , land_cover_short_class), ]
land_cover_agreement[, land_cover_sum := sum(evap_sum), land_cover_short_class]
land_cover_agreement[, land_cover_fraction := evap_sum / land_cover_sum]

### Biome types
biome_class[grepl("Tundra", biome_class) == TRUE, biome_short_class := "Tundra"]
biome_class[grepl("Boreal Forests", biome_class) == TRUE, biome_short_class := "B. Forests"]
biome_class[grepl("Dry Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Dry BL Forests"]
biome_class[grepl("Moist Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Moist BL Forests"]
biome_class[grepl("Subtropical Coniferous Forests", biome_class) == TRUE, biome_short_class := "T/S Coni. Forests"]
biome_class[grepl("Temperate Conifer Forests", biome_class) == TRUE, biome_short_class := "T. Coni. Forests"]
biome_class[grepl("Temperate Broadleaf & Mixed Forests", biome_class) == TRUE, biome_short_class := "T. BL Forests"]
biome_class[grepl("Temperate Grasslands", biome_class) == TRUE, biome_short_class := "T. Grasslands"]
biome_class[grepl("Subtropical Grasslands", biome_class) == TRUE, biome_short_class := "T/S Grasslands"]
biome_class[grepl("Montane Grasslands", biome_class) == TRUE, biome_short_class := "M. Grasslands"]
biome_class[grepl("Flooded", biome_class) == TRUE, biome_short_class := "Flooded"]
biome_class[grepl("Mangroves", biome_class) == TRUE, biome_short_class := "Mangroves"]
biome_class[grepl("Deserts", biome_class) == TRUE, biome_short_class := "Deserts"]
biome_class[grepl("Mediterranean", biome_class) == TRUE, biome_short_class := "Mediterranean"]
biome_class[grepl("N/A", biome_class) == TRUE, biome_short_class := NA]
biome_class[, biome_short_class := factor(biome_short_class)]

biome_evap <- biome_class[, .(evap_sum = sum(evap_volume_year)), .(KG_class_1_name, biome_short_class)]
biome_evap <- biome_evap[complete.cases(biome_evap)]
biome_evap <- biome_evap[order(KG_class_1_name, biome_short_class), ]

biome_agreement <- biome_class[, .(evap_sum = sum(evap_volume_year)), .(evap_quant_dataset_agreement , biome_short_class)]
biome_agreement <- biome_agreement[complete.cases(biome_agreement)]
biome_agreement <- biome_agreement[order(evap_quant_dataset_agreement , biome_short_class), ]
biome_agreement[, biome_sum := sum(evap_sum), biome_short_class]
biome_agreement[, biome_fraction := evap_sum / biome_sum]

### Elevation
elevation_evap <- elevation_class[, .(evap_sum = sum(evap_volume_year)), .(KG_class_1_name, elev_class)]
elevation_evap <- elevation_evap[complete.cases(elevation_evap)]
elevation_evap <- elevation_evap[order(KG_class_1_name, elev_class), ]

elevation_agreement <- elevation_class[, .(evap_sum = sum(evap_volume_year)), .(evap_quant_dataset_agreement , elev_class)]
elevation_agreement <- elevation_agreement[complete.cases(elevation_agreement)]
elevation_agreement <- elevation_agreement[order(evap_quant_dataset_agreement , elev_class), ]
elevation_agreement[, elev_sum := sum(evap_sum), elev_class]
elevation_agreement[, elev_fraction := evap_sum / elev_sum]

### Evaporation quantiles
evap_quant_evap <- evap_quant[, .(evap_sum = sum(evap_volume_year)), .(KG_class_1_name, evap_quant)]
evap_quant_evap <- evap_quant_evap[complete.cases(evap_quant_evap)]
evap_quant_evap <- evap_quant_evap[order(KG_class_1_name, evap_quant), ]

evap_quant_agreement <- evap_quant[, .(evap_sum = sum(evap_volume_year)), .(evap_quant_dataset_agreement , evap_quant)]
evap_quant_agreement <- evap_quant_agreement[complete.cases(evap_quant_agreement)]
evap_quant_agreement <- evap_quant_agreement[order(evap_quant_dataset_agreement , evap_quant), ]
evap_quant_agreement[, evap_quant_sum := sum(evap_sum), evap_quant]
evap_quant_agreement[, evap_quant_fraction := evap_sum / evap_quant_sum]

## Save data
write.csv(partition_KG, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "partition_KG.csv"))
write.csv(partition_KG_sd, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "partition_KG_sd.csv"))
write.csv(partition_KG_datasets[, -2], paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "partition_KG_datasets.csv"))
save(land_cover_evap, land_cover_agreement, biome_evap, biome_agreement, elevation_evap, elevation_agreement, evap_quant_evap, evap_quant_agreement, file = paste0(PATH_SAVE_PARTITION_EVAP, "partition_evap.Rdata"))

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
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

land_cover_agreement$land_cover_short_class <- factor(land_cover_agreement$land_cover_short_class, 
                                                          levels = c("Forests", "Savannas", "Croplands", "Shrublands", "Grasslands", "Water", "Barren", "Snow/Ice", "Other"))
fig_land_cover_partition_fraction <- ggplot(land_cover_agreement[land_cover_short_class != "Other"]) +
  geom_bar(aes(x = land_cover_short_class, y = land_cover_fraction, fill = evap_quant_dataset_agreement ), stat = "identity") +
  xlab('Land cover type')  +
  ylab('Fraction')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = colset_RdBu_5) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

### Biomes
fig_biome_partition_evap_volume <- ggplot(biome_evap) +
  geom_bar(aes(x = reorder(biome_short_class, -(evap_sum)), y = evap_sum, fill = KG_class_1_name), stat = "identity") +
  scale_y_continuous(label = axis_scientific) +
  xlab('Biome class')  +
  ylab(bquote('Evaporation sum ['~km^3~year^-1~']'))  +
  labs(fill = 'Climate type')  +
  scale_fill_manual(values = colset_KG_1_names) +
  theme_light() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#biome_agreement$biome_short_class <- factor(biome_agreement$biome_short_class, 
#                                                  levels = c("T/S Forests", "T/S Grasslands", "T. Forests", 
#                                                             "B. Forests", "T. Grasslands", "Deserts", "Tundra", 
#                                                             "Flooded", "M. Grasslands", "Mediterranean", NA))
fig_biome_partition_fraction <- ggplot(biome_agreement) +
  geom_bar(aes(x = biome_short_class, y = biome_fraction, fill = evap_quant_dataset_agreement ), stat = "identity") +
  xlab('Biome class')  +
  ylab('Fraction')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = colset_RdBu_5) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

### Elevation
fig_elevation_partition_evap_volume <- ggplot(elevation_evap) +
  geom_bar(aes(x = elev_class, y = evap_sum, fill = KG_class_1_name), stat = "identity") +
  scale_y_continuous(label = axis_scientific) +
  xlab('Elevation [m]')  +
  ylab(bquote('Evaporation sum ['~km^3~year^-1~']'))  +
  labs(fill = 'Climate type')  +
  scale_fill_manual(values = colset_KG_1_names) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

fig_elevation_partition_fraction <- ggplot(elevation_agreement) +
  geom_bar(aes(x = elev_class, y = elev_fraction, fill = evap_quant_dataset_agreement ), stat = "identity") +
  xlab('Elevation [m]')  +
  ylab('Fraction')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = colset_RdBu_5) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

### Evaporation classes
fig_evap_partition_evap_volume <- ggplot(evap_quant_evap) +
  geom_bar(aes(x = evap_quant, y = evap_sum, fill = KG_class_1_name), stat = "identity") +
  scale_y_continuous(label = axis_scientific) +
  xlab('Evaporation quantile')  +
  ylab(bquote('Evaporation sum ['~km^3~year^-1~']'))  +
  labs(fill = 'Climate type')  +
  scale_fill_manual(values = colset_KG_1_names) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

fig_evap_partition_fraction <- ggplot(evap_quant_agreement) +
  geom_bar(aes(x = evap_quant, y = evap_quant_fraction, fill = evap_quant_dataset_agreement ), stat = "identity") +
  xlab('Evaporation quantile')  +
  ylab('Fraction')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = colset_RdBu_5) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

### Figure 1
gg_fig_1 <- ggarrange(fig_land_cover_partition_evap_volume, fig_biome_partition_evap_volume, 
                      fig_elevation_partition_evap_volume, fig_evap_partition_evap_volume, 
                    labels = c('a', 'b', 'c', 'd'), align = 'hv',
                    common.legend = T, legend = 'right', 
                    nrow = 2, ncol = 2)
ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "partition_volume_climate_conditioned.png"), width = 10, height = 10)

### Figure 2
gg_fig_2 <- ggarrange(fig_land_cover_partition_fraction,fig_biome_partition_fraction,
                      fig_elevation_partition_fraction, fig_evap_partition_fraction,
                    labels = c('a', 'b', 'c', 'd'), align = 'hv',
                    common.legend = T, legend = 'right', 
                    nrow = 2, ncol = 2)
ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "partition_fraction_agreement_conditioned.png"), width = 10, height = 10)

