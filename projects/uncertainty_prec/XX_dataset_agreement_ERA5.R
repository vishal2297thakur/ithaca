# Investigate the reasons of low dataset agreement
install.packages('tidytext')

library(tidyverse)
library(tidytext)

source('source/partition_prec.R')
source('source/geo_functions.R')
source('source/graphics.R')

# Data
prec_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_datasets.rds"))
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))
ens_stats <- readRDS(paste0(PATH_SAVE_PARTITION_PREC,  "prec_ensemble_stats.rds"))

# Variables
masks_low_agreement <- prec_mask[rel_dataset_agreement == 'low']

datasets_low_agreement <- merge(prec_mask[, .(lon, lat, prec_quant, KG_class_1, 
                                                        elev_class, land_cover_class, biome_class)], prec_datasets, by = c('lon', 'lat'))
#datasets_low_agreement <- merge(masks_low_agreement[, .(lon, lat, prec_quant, KG_class_1, 
#                                                        elev_class, land_cover_class, biome_class)], prec_datasets, by = c('lon', 'lat'))
datasets_low_agreement <- merge(ens_stats[, .(lon, lat, ens_mean_mean)], datasets_low_agreement, by = c('lon', 'lat'))

datasets_low_agreement[, mean_distance := round(ens_mean_mean - prec_mean, 0)]
datasets_low_agreement[ens_mean_mean > 0, rel_mean_distance := round((ens_mean_mean - prec_mean)/ens_mean_mean, 2)]

# Analysis
datasets_low_agreement[, round(mean(abs(rel_mean_distance), na.rm = TRUE), 2), dataset]
dummy <- datasets_low_agreement[, .(rel_abs_diff = round(mean(abs(rel_mean_distance), na.rm = TRUE), 2)), .(dataset, KG_class_1)]
dummy[rel_abs_diff > 0.5]
dummy <- datasets_low_agreement[, .(rel_abs_diff = round(mean(abs(rel_mean_distance), na.rm = TRUE), 2)), .(dataset, elev_class)]
dummy[rel_abs_diff > 0.5]
dummy <- datasets_low_agreement[, .(rel_abs_diff = round(mean(abs(rel_mean_distance), na.rm = TRUE), 2)), .(dataset, biome_class)]
dummy[rel_abs_diff > 0.5]
dummy <- datasets_low_agreement[, .(rel_abs_diff = round(mean(abs(rel_mean_distance), na.rm = TRUE), 2)), .(dataset, land_use_class)]
dummy[rel_abs_diff > 0.5]

# Figures
to_plot <- datasets_low_agreement[, .(rel_mean_distance, KG_class_1, dataset, dataset_type)]
to_plot <- to_plot[, .(mean = mean(rel_mean_distance, na.rm = TRUE), 
                       sd = sd(rel_mean_distance, na.rm = TRUE)), .(KG_class_1, dataset, dataset_type)]
to_plot[, dataset_type := "Other"]
to_plot[dataset == "era5", dataset_type := "ERA5 LAND"]
to_plot[dataset == "cru-ts", dataset_type := "CRU-TS"]
to_plot <- to_plot[order(KG_class_1, mean), ]
to_plot <- to_plot[complete.cases(to_plot)]
to_plot <- to_plot %>% mutate(dataset = tidytext::reorder_within(dataset, mean, within = KG_class_1))

ggplot(to_plot) +
  geom_point(aes(x = mean, y = dataset, col = dataset_type)) +
  geom_vline(xintercept = 0, col = 'grey60') + 
  scale_color_manual(values=c("#E69F00", "#56B4E9", "grey70")) +
  labs(color = 'Dataset', x = "Relative difference from mean", y = "")  +
  tidytext::scale_y_reordered() +
  facet_wrap(vars(KG_class_1), scales = "free") +
  theme_light() +
  theme(strip.background =element_rect(fill = "grey40"))

to_plot <- datasets_low_agreement[, .(rel_mean_distance, elev_class, dataset, dataset_type)]
to_plot <- to_plot[, .(mean = mean(rel_mean_distance, na.rm = TRUE), 
                       sd = sd(rel_mean_distance, na.rm = TRUE)), .(elev_class, dataset, dataset_type)]

ggplot(to_plot) +
  geom_point(aes(x = mean, y = dataset, col = dataset_type)) +
  facet_grid(rows = "elev_class")

to_plot <- datasets_low_agreement[, .(rel_mean_distance, land_use_class, dataset, dataset_type)]
to_plot <- to_plot[, .(mean = mean(rel_mean_distance, na.rm = TRUE), 
                       sd = sd(rel_mean_distance, na.rm = TRUE)), .(land_use_class, dataset, dataset_type)]

ggplot(to_plot) +
  geom_point(aes(x = mean, y = dataset, col = dataset_type)) +
  facet_grid(rows = "land_use_class")
