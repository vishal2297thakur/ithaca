# Partitions annual precipitation (mm and km3) per dataset to different classes 
# and creates the violin plots

install.packages("ggstatsplot")

source('source/partition_prec.R')
source('source/graphics.R')

library(ggthemes)
library(scales)
library(ggstatsplot)

## Data 
prec_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_datasets.rds"))
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))
prec_grid <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_volume_grid.rds"))
prec_dataset_means <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_datasets.rds"))
dataset_types <- unique(prec_dataset_means[, .(dataset, dataset_type)])


## Analysis
prec_datasets_volume <- merge(prec_datasets[dataset %in% PREC_GLOBAL_DATASETS, .(lon, lat, year, dataset, prec)], 
                            prec_grid[, .(lon, lat, area)], 
                            by = c("lon", "lat"), all = TRUE)
prec_datasets_volume[, prec_volume := area * M2_TO_KM2 * prec * MM_TO_KM]
prec_datasets_volume <- prec_datasets_volume[dataset_types, on = .(dataset)]
prec_datasets_volume[, dataset_type := factor(dataset_type, levels =  c("ground stations", "reanalysis", "remote sensing"), 
                              labels = c("Stations", "Reanalyses", "Remote sensing"))]

land_cover_class <- merge(prec_mask[, .(lat, lon, land_cover_short_class)], 
                          prec_datasets_volume[, .(lon, lat, year, prec_volume, area, dataset, dataset_type)], 
                          by = c("lon", "lat"))
land_cover_class_global <- land_cover_class[, .(prec_volume = sum(prec_volume), area = sum(area)), 
                                     .(dataset, dataset_type, land_cover_short_class, year)]
land_cover_class_global[, prec_mean := ((prec_volume / M2_TO_KM2) / area) / MM_TO_KM]

biome_class <- merge(prec_mask[, .(lat, lon, biome_short_class)], 
                          prec_datasets_volume[, .(lon, lat, year, prec_volume, area, dataset, dataset_type)], 
                          by = c("lon", "lat"))
biome_class_global <- biome_class[, .(prec_volume = sum(prec_volume), area = sum(area)), 
                                     .(dataset, dataset_type, biome_short_class, year)]
biome_class_global[, prec_mean := ((prec_volume / M2_TO_KM2) / area) / MM_TO_KM]
biome_class_global <- biome_class_global[complete.cases(biome_class_global)]

elev_class <- merge(prec_mask[, .(lat, lon, elev_class)], 
                          prec_datasets_volume[, .(lon, lat, year, prec_volume, area, dataset, dataset_type)], 
                          by = c("lon", "lat"))
elev_class_global <- elev_class[, .(prec_volume = sum(prec_volume), area = sum(area)), 
                                     .(dataset, dataset_type, elev_class, year)]
elev_class_global[, prec_mean := ((prec_volume / M2_TO_KM2) / area) / MM_TO_KM]

prec_class <- merge(prec_mask[, .(lat, lon, prec_quant)], 
                          prec_datasets_volume[, .(lon, lat, year, prec_volume, area, dataset, dataset_type)], 
                          by = c("lon", "lat"))
prec_class_global <- prec_class[, .(prec_volume = sum(prec_volume), area = sum(area)), 
                                     .(dataset, dataset_type, prec_quant, year)]
prec_class_global[, prec_mean := ((prec_volume / M2_TO_KM2) / area) / MM_TO_KM]

## Tables
table_land_cover_class_mm <- land_cover_class_global[, .(prec_mean = round(mean(prec_mean), 0)), .(land_cover_short_class, dataset_type)]
table_land_cover_class_mm <- dcast(table_land_cover_class_mm, land_cover_short_class ~ dataset_type, value.var = 'prec_mean')
table_land_cover_class_mm <- table_land_cover_class_mm[complete.cases(table_land_cover_class_mm)]
table_land_cover_class_mm_all <- land_cover_class_global[dataset %in% PREC_GLOBAL_DATASETS, .(All = round(mean(prec_mean), 0)), .(land_cover_short_class)]
table_land_cover_class_mm <- table_land_cover_class_mm[table_land_cover_class_mm_all, on = .(land_cover_short_class)]

table_land_cover_class_vol <- land_cover_class_global[, .(prec_volume = round(mean(prec_volume), 0)), .(land_cover_short_class, dataset_type)]
table_land_cover_class_vol <- dcast(table_land_cover_class_vol, land_cover_short_class ~ dataset_type, value.var = 'prec_volume')
table_land_cover_class_vol <- table_land_cover_class_vol[complete.cases(table_land_cover_class_vol)]
table_land_cover_class_vol_all <- land_cover_class_global[dataset %in% PREC_GLOBAL_DATASETS, .(All = round(mean(prec_volume), 0)), .(land_cover_short_class)]
table_land_cover_class_vol <- table_land_cover_class_vol[table_land_cover_class_vol_all, on = .(land_cover_short_class)]

write.csv(table_land_cover_class_mm, paste0(PATH_SAVE_PARTITION_PREC_TABLES, "partition_land_cover_mm.csv"))
write.csv(table_land_cover_class_vol, paste0(PATH_SAVE_PARTITION_PREC_TABLES, "partition_land_cover_vol.csv"))

table_biome_class_mm <- biome_class_global[, .(prec_mean = round(mean(prec_mean), 0)), .(biome_short_class, dataset_type)]
table_biome_class_mm <- dcast(table_biome_class_mm, biome_short_class ~ dataset_type, value.var = 'prec_mean')
table_biome_class_mm <- table_biome_class_mm[complete.cases(table_biome_class_mm)]
table_biome_class_mm_all <- biome_class_global[dataset %in% PREC_GLOBAL_DATASETS, .(All = round(mean(prec_mean), 0)), .(biome_short_class)]
table_biome_class_mm <- table_biome_class_mm[table_biome_class_mm_all, on = .(biome_short_class)]

table_biome_class_vol <- biome_class_global[, .(prec_volume = round(mean(prec_volume), 0)), .(biome_short_class, dataset_type)]
table_biome_class_vol <- dcast(table_biome_class_vol, biome_short_class ~ dataset_type, value.var = 'prec_volume')
table_biome_class_vol <- table_biome_class_vol[complete.cases(table_biome_class_vol)]
table_biome_class_vol_all <- biome_class_global[dataset %in% PREC_GLOBAL_DATASETS, .(All = round(mean(prec_volume), 0)), .(biome_short_class)]
table_biome_class_vol <- table_biome_class_vol[table_biome_class_vol_all, on = .(biome_short_class)]

write.csv(table_biome_class_mm, paste0(PATH_SAVE_PARTITION_PREC_TABLES, "partition_biome_mm.csv"))
write.csv(table_biome_class_vol, paste0(PATH_SAVE_PARTITION_PREC_TABLES, "partition_biome_vol.csv"))

table_elev_class_mm <- elev_class_global[, .(prec_mean = round(mean(prec_mean), 0)), .(elev_class, dataset_type)]
table_elev_class_mm <- dcast(table_elev_class_mm, elev_class ~ dataset_type, value.var = 'prec_mean')
table_elev_class_mm_all <- elev_class_global[dataset %in% PREC_GLOBAL_DATASETS, .(all = round(mean(prec_mean), 0)), .(elev_class)]
table_elev_class_mm <- table_elev_class_mm[table_elev_class_mm_all, on = .(elev_class)]

table_elev_class_vol <- elev_class_global[, .(prec_volume = round(mean(prec_volume), 0)), .(elev_class, dataset_type)]
table_elev_class_vol <- dcast(table_elev_class_vol, elev_class ~ dataset_type, value.var = 'prec_volume')
table_elev_class_vol <- table_elev_class_vol[complete.cases(table_elev_class_vol)]
table_elev_class_vol_all <- elev_class_global[dataset %in% PREC_GLOBAL_DATASETS, .(All = round(mean(prec_volume), 0)), .(elev_class)]
table_elev_class_vol <- table_elev_class_vol[table_elev_class_vol_all, on = .(elev_class)]

write.csv(table_elev_class_mm, paste0(PATH_SAVE_PARTITION_PREC_TABLES, "partition_elev_mm.csv"))
write.csv(table_elev_class_vol, paste0(PATH_SAVE_PARTITION_PREC_TABLES, "partition_elev_vol.csv"))

table_prec_class_mm <- prec_class_global[, .(prec_mean = round(mean(prec_mean), 0)), .(prec_quant, dataset_type)]
table_prec_class_mm <- dcast(table_prec_class_mm, prec_quant ~ dataset_type, value.var = 'prec_mean')
table_prec_class_mm_all <- prec_class_global[dataset %in% PREC_GLOBAL_DATASETS, .(All = round(mean(prec_mean), 0)), .(prec_quant)]
table_prec_class_mm <- table_prec_class_mm[table_prec_class_mm_all, on = .(prec_quant)]
table_prec_class_mm <- table_prec_class_mm[order(prec_quant), ]

table_prec_class_vol <- prec_class_global[, .(prec_volume = round(mean(prec_volume), 0)), .(prec_quant, dataset_type)]
table_prec_class_vol <- dcast(table_prec_class_vol, prec_quant ~ dataset_type, value.var = 'prec_volume')
table_prec_class_vol_all <- prec_class_global[dataset %in% PREC_GLOBAL_DATASETS, .(All = round(mean(prec_volume), 0)), .(prec_quant)]
table_prec_class_vol <- table_prec_class_vol[table_prec_class_vol_all, on = .(prec_quant)]
table_prec_class_vol <- table_prec_class_vol[order(prec_quant), ]

write.csv(table_prec_class_mm, paste0(PATH_SAVE_PARTITION_PREC_TABLES, "partition_prec_mm.csv"))
write.csv(table_prec_class_vol, paste0(PATH_SAVE_PARTITION_PREC_TABLES, "partition_prec_vol.csv"))

## Plots
### Means
ggplot(land_cover_class_global, aes(x = land_cover_short_class, y = prec_mean)) +
  geom_violin(fill = NA, aes(linetype = dataset_type, col = dataset_type), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(width = .2, alpha = .7, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = .05) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Precipitation [mm/year]')) +
  scale_linetype_manual(values = c("solid", "longdash", "dotdash")) +
  scale_color_manual(values = colset_RdBu_5[c(1, 3, 4)]) + 
  facet_wrap(~land_cover_short_class, scales = 'free') +
  guides(col = guide_legend(title = "Dataset type"), lty = guide_legend(title = "Dataset type")) +
  theme_minimal() +
  theme(axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "prec_datasets_land_cover_annual_mm.png"), 
       width = 8, height = 8)

ggplot(biome_class_global, aes(x = biome_short_class, y = prec_mean)) +
  geom_violin(fill = NA, aes(linetype = dataset_type, col = dataset_type), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(width = .2, alpha = .7, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = .05) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Precipitation [mm/year]')) +
  scale_linetype_manual(values = c("solid", "longdash", "dotdash")) +
  scale_color_manual(values = colset_RdBu_5[c(1, 3, 4)]) + 
  facet_wrap(~biome_short_class, scales = 'free') +
  guides(col = guide_legend(title = "Dataset type"), lty = guide_legend(title = "Dataset type")) +
  theme_minimal() +
  theme(axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "supplement/prec_datasets_biome_annual_mm.png"), 
       width = 8, height = 8)

ggplot(elev_class_global, aes(x = elev_class, y = prec_mean)) +
  geom_violin(fill = NA, aes(linetype = dataset_type, col = dataset_type), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(width = .2, alpha = .7, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = .05) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Precipitation [mm/year]')) +
  scale_linetype_manual(values = c("solid", "longdash", "dotdash")) +
  scale_color_manual(values = colset_RdBu_5[c(1, 3, 4)]) + 
  facet_wrap(~elev_class, scales = 'free') +
  guides(col = guide_legend(title = "Dataset type"), lty = guide_legend(title = "Dataset type")) +
  theme_minimal() +
  theme(axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "supplement/prec_datasets_elev_annual_mm.png"), 
       width = 8, height = 8)

ggplot(prec_class_global, aes(x = prec_quant, y = prec_mean )) +
  geom_violin(fill = NA, aes(linetype = dataset_type, col = dataset_type), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(width = .2, alpha = .7, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = .05) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Precipitation [mm/year]')) +
  scale_linetype_manual(values = c("solid", "longdash", "dotdash")) +
  scale_color_manual(values = colset_RdBu_5[c(1, 3, 4)]) + 
  facet_wrap(~prec_quant, scales = 'free') +
  guides(col = guide_legend(title = "Dataset type"), lty = guide_legend(title = "Dataset type")) +
  theme_minimal() +
  theme(axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "supplement/prec_datasets_prec_annual_mm.png"), 
       width = 8, height = 8)

### Volumes
ggplot(land_cover_class_global, aes(x = land_cover_short_class, y = prec_volume)) +
  geom_violin(fill = NA, aes(linetype = dataset_type, col = dataset_type), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(width = .2, alpha = .7, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = .05) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Precipitation ['~km^3~year^-1~']')) +
  scale_linetype_manual(values = c("solid", "longdash", "dotdash")) +
  scale_color_manual(values = colset_RdBu_5[c(1, 3, 4)]) + 
  facet_wrap(~land_cover_short_class, scales = 'free') +
  guides(col = guide_legend(title = "Dataset type"), lty = guide_legend(title = "Dataset type")) +
  theme_minimal() +
  theme(axis.text.x = element_blank())

ggplot(biome_class_global, aes(x = biome_short_class, y = prec_volume )) +
  geom_violin(fill = NA, aes(linetype = dataset_type, col = dataset_type), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(width = .2, alpha = .7, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = .05) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Precipitation ['~km^3~year^-1~']')) +
  scale_linetype_manual(values = c("solid", "longdash", "dotdash")) +  
  scale_color_manual(values = colset_RdBu_5[c(1, 3, 4)]) + 
  facet_wrap(~biome_short_class, scales = 'free') +
  guides(col = guide_legend(title = "Dataset type"), lty = guide_legend(title = "Dataset type")) +
  theme_minimal() +
  theme(axis.text.x = element_blank())

ggplot(elev_class_global, aes(x = elev_class, y = prec_volume )) +
  geom_violin(fill = NA, aes(linetype = dataset_type, col = dataset_type), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(width = .2, alpha = .7, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = .05) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Precipitation ['~km^3~year^-1~']')) +
  scale_linetype_manual(values = c("solid", "longdash", "dotdash")) +  
  scale_color_manual(values = colset_RdBu_5[c(1, 3, 4)]) + 
  facet_wrap(~elev_class, scales = 'free') +
  guides(col = guide_legend(title = "Dataset type"), lty = guide_legend(title = "Dataset type")) +
  theme_minimal() +
  theme(axis.text.x = element_blank())

ggplot(prec_class_global, aes(x = prec_quant, y = prec_volume)) +
  geom_violin(fill = NA, aes(linetype = dataset_type, col = dataset_type), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(width = .2, alpha = .7, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = .05) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Precipitation ['~km^3~year^-1~']')) +
  scale_linetype_manual(values = c("solid", "longdash", "dotdash")) +  
  scale_color_manual(values = colset_RdBu_5[c(1, 3, 4)]) + 
  facet_wrap(~prec_quant, scales = 'free') +
  guides(col = guide_legend(title = "Dataset type"), lty = guide_legend(title = "Dataset type")) +
  theme_minimal() +
  theme(axis.text.x = element_blank())

