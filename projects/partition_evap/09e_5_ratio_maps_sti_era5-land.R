# Ratio of standard quantile range between groups ----

source('source/partition_evap.R')
source('source/geo_functions.R')
source('source/graphics.R')

## Packages ----
library("gtools")

## Data ----
evap_stats_cold <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_cold_sti_era5-land.rds"))
evap_stats_warm <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_warm_sti_era5-land.rds"))

## Analysis ----
evap_stats <- merge(evap_stats_cold, evap_stats_warm, by = c("lon", "lat"), suffixes = c(".cold", ".warm"))

evap_stats[, ratio_std_quantile := std_quant_range.cold/std_quant_range.warm]
evap_stats[, ratio_std_quantile_brk := cut(ratio_std_quantile, breaks = c(0, 0.5, 0.9, 1.11, 2, 500))]

## Figure ----
ggplot(evap_stats)+
  geom_tile(aes(x = lon, y = lat, fill = ratio_std_quantile_brk, col = ratio_std_quantile_brk))+
  scale_fill_manual(values = c("darkred", "firebrick1","gray70", "skyblue1", "darkblue"))+
  scale_color_manual(values = c("darkred", "firebrick1","gray70", "skyblue1", "darkblue"), guide = "none")+
  labs(fill = "cold /warm")+
  ggtitle("Ratio of standard quantile range")+
  theme_bw()
ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "ratio_std_quantile_sti_era5-land_cold_warm.png"), 
       width = 16, height = 8)

## Data ----
evap_stats_cold <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_cold_rank_sti_era5-land.rds"))
evap_stats_warm <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_warm_rank_sti_era5-land.rds"))

## Analysis ----
evap_stats <- merge(evap_stats_cold, evap_stats_warm, by = c("lon", "lat"), suffixes = c(".cold", ".warm"))

evap_stats[, ratio_std_quantile := std_quant_range.cold/std_quant_range.warm]
evap_stats[, ratio_std_quantile_brk := cut(ratio_std_quantile, breaks = c(0, 0.5, 0.9, 1.11, 2, 500))]

## Figure ----
ggplot(evap_stats)+
  geom_tile(aes(x = lon, y = lat, fill = ratio_std_quantile_brk, col = ratio_std_quantile_brk))+
  scale_fill_manual(values = c("darkred", "firebrick1","gray70", "skyblue1", "darkblue"))+
  scale_color_manual(values = c("darkred", "firebrick1","gray70", "skyblue1", "darkblue"), guide = "none")+
  labs(fill = "cold /warm")+
  ggtitle("Ratio of standard quantile range")+
  theme_bw()
ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "ratio_std_quantile_sti_era5-land_cold_warm_rank.png"), 
       width = 16, height = 8)
