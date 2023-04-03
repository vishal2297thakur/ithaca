# Partition precipitation to different regional properties and quantify their uncertainty
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

## Analysis
prec_datasets_volume <- merge(prec_datasets[, .(lon, lat, year, dataset, prec)], 
                            prec_grid[, .(lon, lat, area)], 
                            by = c("lon", "lat"), all = TRUE)
prec_datasets_volume[, prec_volume := area  * M2_TO_KM2 * prec * MM_TO_KM
                     ][, prec := NULL
                       ][, area := NULL]     

land_cover_class <- merge(prec_mask[, .(lat, lon, land_cover_short_class)], 
                          prec_datasets_volume[, .(lon, lat, year, prec_volume, dataset)], 
                          by = c("lon", "lat"))
land_cover_class_global <- land_cover_class[, .(prec_volume = sum(prec_volume)), 
                                     .(dataset, land_cover_short_class, year)]

land_cover_class <- merge(prec_mask[, .(lat, lon, land_cover_short_class)], 
                          prec_datasets_volume[, .(lon, lat, year, prec_volume, dataset)], 
                          by = c("lon", "lat"))
land_cover_class_global <- land_cover_class[, .(prec_volume = sum(prec_volume)), 
                                     .(dataset, land_cover_short_class, year)]

## Plots
### Land use
ggplot(land_cover_class_global, aes(x = land_cover_short_class, y = prec_volume )) +
  geom_violin(fill = NA) +
  geom_boxplot(width = .2, alpha = .6, fatten = NULL, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = .1) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = F, 
               position = position_dodge(.175)) +
  scale_x_discrete(name = "Land cover class") +
  scale_y_continuous(name = "Volume (km3)") +
  facet_wrap(~land_cover_short_class, scales = 'free') +
  theme_minimal()
