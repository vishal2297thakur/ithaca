# Ratio of standard quantile range between groups ----

source('source/partition_evap.R')
source('source/geo_functions.R')
source('source/graphics.R')

## Packages ----
library("gtools")

## Data ----
evap_stats_el_nino <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_ENSO_el_nino.rds"))
evap_stats_la_nina <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_ENSO_la_nina.rds"))

## Analysis ----

evap_stats <- merge(evap_stats_el_nino, evap_stats_la_nina, by = c("lon", "lat"), suffixes = c(".el_nino", ".la_nina"))

evap_stats[, ratio_std_quantile := std_quant_range.el_nino/std_quant_range.la_nina]
evap_stats[, ratio_std_quantile_brk := cut(ratio_std_quantile, breaks = c(0, 0.5, 0.9, 1.11, 2, 500))]

## Figure ----

ggplot(evap_stats)+
  geom_tile(aes(x = lon, y = lat, fill = ratio_std_quantile_brk, col = ratio_std_quantile_brk))+
  scale_fill_manual(values = c("darkred", "firebrick1","gray70", "skyblue1", "darkblue"))+
  scale_color_manual(values = c("darkred", "firebrick1","gray70", "skyblue1", "darkblue"), guide = "none")+
  labs(fill = "El nino /La nina")+
  ggtitle("Ratio of standard quantile range")+
  theme_bw()
ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "ratio_std_quantile_el_nino_la_nina.png"), 
       width = 16, height = 8)
