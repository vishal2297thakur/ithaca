# Partitioning of annual precipitation [km3 and mm] to different grid cell types 
# presented by violin plots 

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
prec_datasets_volume <- merge(prec_datasets[, .(lon, lat, year, dataset, prec)], 
                            prec_grid[, .(lon, lat, area)], 
                            by = c("lon", "lat"), all = TRUE)
prec_datasets_volume[, prec_volume := area * M2_TO_KM2 * prec * MM_TO_KM]
prec_datasets_volume <- prec_datasets_volume[dataset_types, on = .(dataset)]

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
table_land_cover_class_mm <- dcast(table_land_cover_class_mm, land_cover_short_class  ~ dataset_type, value.var = 'prec_mean')
table_land_cover_class_mm <- table_land_cover_class_mm[complete.cases(table_land_cover_class_mm)]
table_land_cover_class_mm_all <- land_cover_class_global[, .(all = round(mean(prec_mean), 0)), .(land_cover_short_class)]
table_land_cover_class_mm <- table_land_cover_class_mm[table_land_cover_class_mm_all, on = .(land_cover_short_class)]

table_land_cover_class_vol <- land_cover_class_global[, .(prec_volume = round(mean(prec_volume), 0)), .(land_cover_short_class, dataset_type)]
table_land_cover_class_vol <- dcast(table_land_cover_class_vol, land_cover_short_class  ~ dataset_type, value.var = 'prec_volume')
table_land_cover_class_vol <- table_land_cover_class_vol[complete.cases(table_land_cover_class_vol)]
table_land_cover_class_vol_all <- land_cover_class_global[, .(all = round(mean(prec_volume), 0)), .(land_cover_short_class)]
table_land_cover_class_vol <- table_land_cover_class_vol[table_land_cover_class_vol_all, on = .(land_cover_short_class)]

write.csv(table_land_cover_class_mm, paste0(PATH_SAVE_PARTITION_PREC_TABLES, "partition_land_cover_mm.csv"))
write.csv(table_land_cover_class_vol, paste0(PATH_SAVE_PARTITION_PREC_TABLES, "partition_land_cover_vol.csv"))

## Plots
### Volumes
ggplot(land_cover_class_global, aes(x = land_cover_short_class, y = prec_volume)) +
  geom_violin(fill = NA, aes(linetype = dataset_type, col = dataset_type), position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(width = .2, alpha = .7, fatten = NULL, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = .05) +
  stat_table(fun.data = "mean_se", geom = "pointrange", show.legend = F, 
               position = position_dodge(.175)) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Precipitation ['~km^3~year^-1~']')) +
  scale_linetype_manual(values = c("solid", "longdash", "dotdash")) +
  scale_color_manual(values = colset_RdBu_5[c(1, 3, 4)]) + 
  facet_wrap(~land_cover_short_class, scales = 'free') +
  theme_minimal() 
ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "prec_datasets_land_cover_annual_vol.png"), 
       width = 8, height = 5)

ggplot(biome_class_global, aes(x = biome_short_class, y = prec_volume )) +
  geom_violin(fill = NA, aes(linetype = dataset_type, col = dataset_type), position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(width = .2, alpha = .7, fatten = NULL, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = .05) +
  stat_table(fun.data = "mean_se", geom = "pointrange", show.legend = F, 
               position = position_dodge(.175)) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Precipitation ['~km^3~year^-1~']')) +
  scale_linetype_manual(values = c("solid", "longdash", "dotdash")) +  
  facet_wrap(~biome_short_class, scales = 'free') +
  theme_minimal() 
ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "supplement/prec_datasets_biome_annual_vol.png"), 
       width = 8, height = 5)

ggplot(elev_class_global, aes(x = elev_class, y = prec_volume )) +
  geom_violin(fill = NA, aes(linetype = dataset_type, col = dataset_type), position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(width = .2, alpha = .7, fatten = NULL, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = .05) +
  stat_table(fun.data = "mean_se", geom = "pointrange", show.legend = F, 
               position = position_dodge(.175)) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Precipitation ['~km^3~year^-1~']')) +
  scale_linetype_manual(values = c("solid", "longdash", "dotdash")) +  
  facet_wrap(~elev_class, scales = 'free') +
  theme_minimal() +
  theme(axis.text.x = element_blank()) 
ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "supplement/prec_datasets_elev_annual_vol.png"), 
       width = 8, height = 5)

ggplot(prec_class_global, aes(x = prec_quant, y = prec_volume)) +
  geom_violin(fill = NA, aes(linetype = dataset_type, col = dataset_type), position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(width = .2, alpha = .7, fatten = NULL, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = .05) +
  stat_table(fun.data = "mean_se", geom = "pointrange", show.legend = F, 
               position = position_dodge(.175)) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Precipitation ['~km^3~year^-1~']')) +
  scale_linetype_manual(values = c("solid", "longdash", "dotdash")) +  
  facet_wrap(~prec_quant, scales = 'free') +
  theme_minimal() +
  theme(axis.text.x = element_blank()) 
ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "supplement/prec_datasets_prec_annual_vol.png"), 
       width = 8, height = 5)

### Means
ggplot(land_cover_class_global, aes(x = land_cover_short_class, y = prec_mean)) +
  geom_violin(fill = NA, aes(linetype = dataset_type, col = dataset_type), position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(width = .2, alpha = .7, fatten = NULL, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = .05) +
  stat_table(fun.data = "mean_se", geom = "pointrange", show.legend = F, 
               position = position_dodge(.175)) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Precipitation [mm]')) +
  scale_linetype_manual(values = c("solid", "longdash", "dotdash")) +
  scale_color_manual(values = colset_RdBu_5[c(1, 3, 4)]) + 
  facet_wrap(~land_cover_short_class, scales = 'free') +
  theme_minimal() 
ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "prec_datasets_land_cover_annual_mm.png"), 
       width = 8, height = 5)

ggplot(biome_class_global, aes(x = biome_short_class, y = prec_mean)) +
  geom_violin(fill = NA, aes(linetype = dataset_type, col = dataset_type), position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(width = .2, alpha = .7, fatten = NULL, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = .05) +
  stat_table(fun.data = "mean_se", geom = "pointrange", show.legend = F, 
               position = position_dodge(.175)) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Precipitation [mm]')) +
  scale_linetype_manual(values = c("solid", "longdash", "dotdash")) +
  scale_color_manual(values = colset_RdBu_5[c(1, 3, 4)]) + 
  facet_wrap(~biome_short_class, scales = 'free') +
  theme_minimal() 
ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "supplement/prec_datasets_biome_annual_mm.png"), 
       width = 8, height = 5)

ggplot(elev_class_global, aes(x = elev_class, y = prec_mean)) +
  geom_violin(fill = NA, aes(linetype = dataset_type, col = dataset_type), position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(width = .2, alpha = .7, fatten = NULL, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = .05) +
  stat_table(fun.data = "mean_se", geom = "pointrange", show.legend = F, 
               position = position_dodge(.175)) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Precipitation [mm]')) +
  scale_linetype_manual(values = c("solid", "longdash", "dotdash")) +
  scale_color_manual(values = colset_RdBu_5[c(1, 3, 4)]) + 
  facet_wrap(~elev_class, scales = 'free') +
  theme_minimal() 
ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "supplement/prec_datasets_elev_annual_mm.png"), 
       width = 8, height = 5)

ggplot(prec_class_global, aes(x = prec_quant, y = prec_mean )) +
  geom_violin(fill = NA, aes(linetype = dataset_type, col = dataset_type), position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(width = .2, alpha = .7, fatten = NULL, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = .05) +
  stat_table(fun.data = "mean_se", geom = "pointrange", show.legend = F, 
               position = position_dodge(.175)) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Precipitation [mm]')) +
  scale_linetype_manual(values = c("solid", "longdash", "dotdash")) +
  scale_color_manual(values = colset_RdBu_5[c(1, 3, 4)]) + 
  facet_wrap(~prec_quant, scales = 'free') +
  theme_minimal() 
ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "supplement/prec_datasets_prec_annual_mm.png"), 
       width = 8, height = 5)


