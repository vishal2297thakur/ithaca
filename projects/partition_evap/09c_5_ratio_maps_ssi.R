# Addition of categorical classes to each grid cell ----

source('source/partition_evap.R')
source('source/geo_functions.R')
source('source/graphics.R')

## Packages ----
library("gtools")

## Data ----
evap_stats_dry <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_dry_ssi_era5-land.rds"))
evap_stats_wet <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_wet_ssi_era5-land.rds"))

evap_stats <- merge(evap_stats_dry, evap_stats_wet, by = c("lon", "lat"), suffixes = c(".dry", ".wet"))

evap_stats[, ratio_std_quantile := std_quant_range.dry/std_quant_range.wet]
evap_stats[, ratio_std_quantile_brk := cut(ratio_std_quantile, breaks = c(0, 0.5, 0.9, 1.11, 2, 500))]

ggplot(evap_stats)+
  geom_tile(aes(x = lon, y = lat, fill = ratio_std_quantile_brk, col = ratio_std_quantile_brk))+
  scale_fill_manual(values = c("darkred", "firebrick1","gray70", "skyblue1", "darkblue"))+
  scale_color_manual(values = c("darkred", "firebrick1","gray70", "skyblue1", "darkblue"), guide = "none")+
  labs(fill = "Dry /Wet")+
  ggtitle("Ratio of standard quantile range")+
  theme_bw()
ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "ratio_std_quantile_ssi_era5-land_dry_wet.png"), 
       width = 16, height = 8)
