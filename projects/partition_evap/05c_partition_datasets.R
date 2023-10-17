# Partitions annual evaporation (mm and km3) per dataset to different classes 
# and creates the violin plots

install.packages("ggstatsplot")

source('source/partition_evap.R')
source('source/graphics.R')

library(ggthemes)
library(scales)
library(ggstatsplot)

## Data 
evap_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets.rds"))
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
evap_grid <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_volume_grid.rds"))
evap_dataset_means <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_datasets.rds"))
dataset_types <- unique(evap_dataset_means[, .(dataset, dataset_type)])


## Analysis
evap_datasets_volume <- merge(evap_datasets[dataset %in% EVAP_GLOBAL_DATASETS, .(lon, lat, year, dataset, evap)], 
                            evap_grid[, .(lon, lat, area)], 
                            by = c("lon", "lat"), all = TRUE)
evap_datasets_volume[, evap_volume := area * M2_TO_KM2 * evap * MM_TO_KM]
evap_datasets_volume <- evap_datasets_volume[dataset_types, on = .(dataset)]
evap_datasets_volume[, dataset_type := factor(dataset_type, levels =  c( "reanalysis", "remote sensing","hydrologic model","ensemble"), 
                              labels = c( "Reanalyses", "Remote sensing","Hydrologic model","Ensemble"))]

land_cover_class <- merge(evap_mask[, .(lat, lon, land_cover_short_class)], 
                          evap_datasets_volume[, .(lon, lat, year, evap_volume, area, dataset, dataset_type)], 
                          by = c("lon", "lat"))
land_cover_class_global <- land_cover_class[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                     .(dataset, dataset_type, land_cover_short_class, year)]
land_cover_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]

biome_class <- merge(evap_mask[, .(lat, lon, biome_short_class)], 
                          evap_datasets_volume[, .(lon, lat, year, evap_volume, area, dataset, dataset_type)], 
                          by = c("lon", "lat"))
biome_class_global <- biome_class[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                     .(dataset, dataset_type, biome_short_class, year)]
biome_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]
biome_class_global <- biome_class_global[complete.cases(biome_class_global)]

elev_class <- merge(evap_mask[, .(lat, lon, elev_class)], 
                          evap_datasets_volume[, .(lon, lat, year, evap_volume, area, dataset, dataset_type)], 
                          by = c("lon", "lat"))
elev_class_global <- elev_class[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                     .(dataset, dataset_type, elev_class, year)]
elev_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]

evap_class <- merge(evap_mask[, .(lat, lon, evap_quant)], 
                          evap_datasets_volume[, .(lon, lat, year, evap_volume, area, dataset, dataset_type)], 
                          by = c("lon", "lat"))
evap_class_global <- evap_class[, .(evap_volume = sum(evap_volume), area = sum(area)), 
                                     .(dataset, dataset_type, evap_quant, year)]
evap_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]

## Tables
table_land_cover_class_mm <- land_cover_class_global[, .(evap_mean = round(mean(evap_mean), 0)), .(land_cover_short_class, dataset_type)]
table_land_cover_class_mm <- dcast(table_land_cover_class_mm, land_cover_short_class ~ dataset_type, value.var = 'evap_mean')
table_land_cover_class_mm <- table_land_cover_class_mm[complete.cases(table_land_cover_class_mm)]
table_land_cover_class_mm_all <- land_cover_class_global[dataset %in% EVAP_GLOBAL_DATASETS, .(All = round(mean(evap_mean), 0)), .(land_cover_short_class)]
table_land_cover_class_mm <- table_land_cover_class_mm[table_land_cover_class_mm_all, on = .(land_cover_short_class)]

table_land_cover_class_vol <- land_cover_class_global[, .(evap_volume = round(mean(evap_volume), 0)), .(land_cover_short_class, dataset_type)]
table_land_cover_class_vol <- dcast(table_land_cover_class_vol, land_cover_short_class ~ dataset_type, value.var = 'evap_volume')
table_land_cover_class_vol <- table_land_cover_class_vol[complete.cases(table_land_cover_class_vol)]
table_land_cover_class_vol_all <- land_cover_class_global[dataset %in% EVAP_GLOBAL_DATASETS, .(All = round(mean(evap_volume), 0)), .(land_cover_short_class)]
table_land_cover_class_vol <- table_land_cover_class_vol[table_land_cover_class_vol_all, on = .(land_cover_short_class)]

write.csv(table_land_cover_class_mm, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "partition_land_cover_mm.csv"))
write.csv(table_land_cover_class_vol, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "partition_land_cover_vol.csv"))

table_biome_class_mm <- biome_class_global[, .(evap_mean = round(mean(evap_mean), 0)), .(biome_short_class, dataset_type)]
table_biome_class_mm <- dcast(table_biome_class_mm, biome_short_class ~ dataset_type, value.var = 'evap_mean')
table_biome_class_mm <- table_biome_class_mm[complete.cases(table_biome_class_mm)]
table_biome_class_mm_all <- biome_class_global[dataset %in% EVAP_GLOBAL_DATASETS, .(All = round(mean(evap_mean), 0)), .(biome_short_class)]
table_biome_class_mm <- table_biome_class_mm[table_biome_class_mm_all, on = .(biome_short_class)]

table_biome_class_vol <- biome_class_global[, .(evap_volume = round(mean(evap_volume), 0)), .(biome_short_class, dataset_type)]
table_biome_class_vol <- dcast(table_biome_class_vol, biome_short_class ~ dataset_type, value.var = 'evap_volume')
table_biome_class_vol <- table_biome_class_vol[complete.cases(table_biome_class_vol)]
table_biome_class_vol_all <- biome_class_global[dataset %in% EVAP_GLOBAL_DATASETS, .(All = round(mean(evap_volume), 0)), .(biome_short_class)]
table_biome_class_vol <- table_biome_class_vol[table_biome_class_vol_all, on = .(biome_short_class)]

write.csv(table_biome_class_mm, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "partition_biome_mm.csv"))
write.csv(table_biome_class_vol, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "partition_biome_vol.csv"))

table_elev_class_mm <- elev_class_global[, .(evap_mean = round(mean(evap_mean), 0)), .(elev_class, dataset_type)]
table_elev_class_mm <- dcast(table_elev_class_mm, elev_class ~ dataset_type, value.var = 'evap_mean')
table_elev_class_mm_all <- elev_class_global[dataset %in% EVAP_GLOBAL_DATASETS, .(all = round(mean(evap_mean), 0)), .(elev_class)]
table_elev_class_mm <- table_elev_class_mm[table_elev_class_mm_all, on = .(elev_class)]

table_elev_class_vol <- elev_class_global[, .(evap_volume = round(mean(evap_volume), 0)), .(elev_class, dataset_type)]
table_elev_class_vol <- dcast(table_elev_class_vol, elev_class ~ dataset_type, value.var = 'evap_volume')
table_elev_class_vol <- table_elev_class_vol[complete.cases(table_elev_class_vol)]
table_elev_class_vol_all <- elev_class_global[dataset %in% EVAP_GLOBAL_DATASETS, .(All = round(mean(evap_volume), 0)), .(elev_class)]
table_elev_class_vol <- table_elev_class_vol[table_elev_class_vol_all, on = .(elev_class)]

write.csv(table_elev_class_mm, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "partition_elev_mm.csv"))
write.csv(table_elev_class_vol, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "partition_elev_vol.csv"))

table_evap_class_mm <- evap_class_global[, .(evap_mean = round(mean(evap_mean), 0)), .(evap_quant, dataset_type)]
table_evap_class_mm <- dcast(table_evap_class_mm, evap_quant ~ dataset_type, value.var = 'evap_mean')
table_evap_class_mm_all <- evap_class_global[dataset %in% EVAP_GLOBAL_DATASETS, .(All = round(mean(evap_mean), 0)), .(evap_quant)]
table_evap_class_mm <- table_evap_class_mm[table_evap_class_mm_all, on = .(evap_quant)]
table_evap_class_mm <- table_evap_class_mm[order(evap_quant), ]

table_evap_class_vol <- evap_class_global[, .(evap_volume = round(mean(evap_volume), 0)), .(evap_quant, dataset_type)]
table_evap_class_vol <- dcast(table_evap_class_vol, evap_quant ~ dataset_type, value.var = 'evap_volume')
table_evap_class_vol_all <- evap_class_global[dataset %in% EVAP_GLOBAL_DATASETS, .(All = round(mean(evap_volume), 0)), .(evap_quant)]
table_evap_class_vol <- table_evap_class_vol[table_evap_class_vol_all, on = .(evap_quant)]
table_evap_class_vol <- table_evap_class_vol[order(evap_quant), ]

write.csv(table_evap_class_mm, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "partition_evap_mm.csv"))
write.csv(table_evap_class_vol, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "partition_evap_vol.csv"))

## Plots
### Means
ggplot(land_cover_class_global, aes(x = land_cover_short_class, y = evap_mean)) +
  geom_violin(fill = NA, aes(linetype = dataset_type, col = dataset_type), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(width = .2, alpha = .7, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = .05) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Evaporation [mm/year]')) +
  scale_linetype_manual(values = c("solid", "longdash","solid","dotdash")) +
  scale_color_manual(values = colset_RdBu_5[c(1, 2, 3, 4)]) + 
  facet_wrap(~land_cover_short_class, scales = 'free') +
  guides(col = guide_legend(title = "Dataset type"), lty = guide_legend(title = "Dataset type")) +
  theme_minimal() +
  theme(axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "evap_datasets_land_cover_annual_mm.png"), 
       width = 8, height = 8)

ggplot(biome_class_global, aes(x = biome_short_class, y = evap_mean)) +
  geom_violin(fill = NA, aes(linetype = dataset_type, col = dataset_type), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(width = .2, alpha = .7, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = .05) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Evaporation [mm/year]')) +
  scale_linetype_manual(values = c("solid", "longdash","solid","dotdash")) +
  scale_color_manual(values = colset_RdBu_5[c(1,2, 3, 4)]) + 
  facet_wrap(~biome_short_class, scales = 'free') +
  guides(col = guide_legend(title = "Dataset type"), lty = guide_legend(title = "Dataset type")) +
  theme_minimal() +
  theme(axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/evap_datasets_biome_annual_mm.png"), 
       width = 8, height = 8)

ggplot(elev_class_global, aes(x = elev_class, y = evap_mean)) +
  geom_violin(fill = NA, aes(linetype = dataset_type, col = dataset_type), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(width = .2, alpha = .7, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = .05) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Evaporation [mm/year]')) +
  scale_linetype_manual(values = c("solid", "longdash","solid","dotdash")) +
  scale_color_manual(values = colset_RdBu_5[c(1,2, 3, 4)]) + 
  facet_wrap(~elev_class, scales = 'free') +
  guides(col = guide_legend(title = "Dataset type"), lty = guide_legend(title = "Dataset type")) +
  theme_minimal() +
  theme(axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/evap_datasets_elev_annual_mm.png"), 
       width = 8, height = 8)

ggplot(evap_class_global, aes(x = evap_quant, y = evap_mean )) +
  geom_violin(fill = NA, aes(linetype = dataset_type, col = dataset_type), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(width = .2, alpha = .7, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = .05) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Evaporation [mm/year]')) +
  scale_linetype_manual(values = c("solid", "longdash","solid","dotdash")) +
  scale_color_manual(values = colset_RdBu_5[c(1,2, 3, 4)]) + 
  facet_wrap(~evap_quant, scales = 'free') +
  guides(col = guide_legend(title = "Dataset type"), lty = guide_legend(title = "Dataset type")) +
  theme_minimal() +
  theme(axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/evap_datasets_evap_annual_mm.png"), 
       width = 8, height = 8)

### Volumes
ggplot(land_cover_class_global, aes(x = land_cover_short_class, y = evap_volume)) +
  geom_violin(fill = NA, aes(linetype = dataset_type, col = dataset_type), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(width = .2, alpha = .7, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = .05) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Evaporation ['~km^3~year^-1~']')) +
  scale_linetype_manual(values = c("solid", "longdash","solid","dotdash")) +
  scale_color_manual(values = colset_RdBu_5[c(1, 2,3, 4)]) + 
  facet_wrap(~land_cover_short_class, scales = 'free') +
  guides(col = guide_legend(title = "Dataset type"), lty = guide_legend(title = "Dataset type")) +
  theme_minimal() +
  theme(axis.text.x = element_blank())

ggplot(biome_class_global, aes(x = biome_short_class, y = evap_volume )) +
  geom_violin(fill = NA, aes(linetype = dataset_type, col = dataset_type), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(width = .2, alpha = .7, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = .05) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Evaporation ['~km^3~year^-1~']')) +
  scale_linetype_manual(values = c("solid", "longdash","solid","dotdash")) +  
  scale_color_manual(values = colset_RdBu_5[c(1,2, 3, 4)]) + 
  facet_wrap(~biome_short_class, scales = 'free') +
  guides(col = guide_legend(title = "Dataset type"), lty = guide_legend(title = "Dataset type")) +
  theme_minimal() +
  theme(axis.text.x = element_blank())

ggplot(elev_class_global, aes(x = elev_class, y = evap_volume )) +
  geom_violin(fill = NA, aes(linetype = dataset_type, col = dataset_type), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(width = .2, alpha = .7, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = .05) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Evaporation ['~km^3~year^-1~']')) +
  scale_linetype_manual(values = c("solid", "longdash","solid","dotdash")) +  
  scale_color_manual(values = colset_RdBu_5[c(1,2, 3, 4)]) + 
  facet_wrap(~elev_class, scales = 'free') +
  guides(col = guide_legend(title = "Dataset type"), lty = guide_legend(title = "Dataset type")) +
  theme_minimal() +
  theme(axis.text.x = element_blank())

ggplot(evap_class_global, aes(x = evap_quant, y = evap_volume)) +
  geom_violin(fill = NA, aes(linetype = dataset_type, col = dataset_type), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(width = .2, alpha = .7, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = .05) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Evaporation ['~km^3~year^-1~']')) +
  scale_linetype_manual(values = c("solid", "longdash","solid","dotdash")) +  
  scale_color_manual(values = colset_RdBu_5[c(1,2, 3, 4)]) + 
  facet_wrap(~evap_quant, scales = 'free') +
  guides(col = guide_legend(title = "Dataset type"), lty = guide_legend(title = "Dataset type")) +
  theme_minimal() +
  theme(axis.text.x = element_blank())

