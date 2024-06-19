# Ratio of standard quantile range between groups for different mask categories ----

source('source/partition_evap.R')
source('source/geo_functions.R')
source('source/graphics.R')

## Packages ----
library("gtools")

## Read Data ----
### Masks ----
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))

### Stats data based on spi ----
evap_stats_dry <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_dry_spi.rds"))
evap_stats_wet <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_wet_spi.rds"))

## Analysis ----
evap_stats <- merge(evap_stats_dry, evap_stats_wet, by = c("lon", "lat"), suffixes = c(".dry", ".wet"))

evap_stats[, ratio_std_quantile := std_quant_range.dry/std_quant_range.wet]
evap_stats[, ratio_std_quantile_brk := cut(ratio_std_quantile, breaks = c(0, 0.9, 1.11,500))]


## Analysis ----
evap_stats[, ratio_std_quantile_brk := factor(ratio_std_quantile_brk, levels = c("(0,0.9]", "(0.9,1.11]", "(1.11,500]"),
                                              labels = c("SQR wet > SQR dry",
                                                         "SQR dry = SQR wet",
                                                         "SQR dry > SQR wet"), 
                                              ordered = T),]
evap_stats_masks <- merge(evap_stats, evap_mask, all.x = T, by = c("lon", "lat"))
grid_cell_area <- unique(evap_stats[, .(lon, lat)]) %>% grid_area() # m2
evap_stats_masks <- grid_cell_area[evap_stats_masks, on = .(lon, lat)]

### Biome types ----
biome_stats <- evap_stats_masks[,.(sqr_group_area = sum(area)),.(ratio_std_quantile_brk, biome_class)]
biome_stats <- biome_stats[complete.cases(biome_stats)]
biome_stats[, biome_area:= sum(sqr_group_area), .(biome_class)]
biome_stats[, biome_fraction:= sqr_group_area/biome_area]
biome_stats[grepl("Tundra", biome_class) == TRUE, biome_short_class := "Tundra"]
biome_stats[grepl("Boreal Forests", biome_class) == TRUE, biome_short_class := "B. Forests"]
biome_stats[grepl("Dry Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Dry BL Forests"]
biome_stats[grepl("Moist Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Moist BL Forests"]
biome_stats[grepl("Subtropical Coniferous Forests", biome_class) == TRUE, biome_short_class := "T/S Coni. Forests"]
biome_stats[grepl("Temperate Conifer Forests", biome_class) == TRUE, biome_short_class := "T. Coni. Forests"]
biome_stats[grepl("Temperate Broadleaf & Mixed Forests", biome_class) == TRUE, biome_short_class := "T. BL Forests"]
biome_stats[grepl("Temperate Grasslands", biome_class) == TRUE, biome_short_class := "T. Grasslands"]
biome_stats[grepl("Subtropical Grasslands", biome_class) == TRUE, biome_short_class := "T/S Grasslands"]
biome_stats[grepl("Montane Grasslands", biome_class) == TRUE, biome_short_class := "M. Grasslands"]
biome_stats[grepl("Flooded", biome_class) == TRUE, biome_short_class := "Flooded"]
biome_stats[grepl("Mangroves", biome_class) == TRUE, biome_short_class := "Mangroves"]
biome_stats[grepl("Deserts", biome_class) == TRUE, biome_short_class := "Deserts"]
biome_stats[grepl("Mediterranean", biome_class) == TRUE, biome_short_class := "Mediterranean"]
biome_stats[grepl("N/A", biome_class) == TRUE, biome_short_class := NA]
biome_stats[, biome_short_class := factor(biome_short_class)]
biome_stats <- biome_stats[complete.cases(biome_stats)]

#### plot biome trend direction ----
ggplot(biome_stats) +
  geom_bar(aes(x = biome_short_class, y = biome_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('Biome')  +
  ylab('Area fraction')  +
  labs(fill = 'Trend direction')  +
  scale_fill_manual(values = c("SQR dry > SQR wet" = colset_RdBu_5[1], 
                               "SQR wet > SQR dry" = colset_RdBu_5[5],
                               "SQR dry = SQR wet" = "gray90"))+
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "bar_biome_cumulative_fraction_SQR_ratio_spi.png"), 
       width = 8, height = 8)

### ipcc ref regions  ----
ipcc_statss <- evap_stats_masks[,.(stats_area = sum(area)),.(ratio_std_quantile_brk, IPCC_ref_region)]
ipcc_statss <- ipcc_statss[complete.cases(ipcc_statss)]
ipcc_statss[, ipcc_area:= sum(stats_area), .(IPCC_ref_region)]
ipcc_statss[, ipcc_fraction:= stats_area/ipcc_area]


ggplot(ipcc_statss) +
  geom_bar(aes(x = IPCC_ref_region, y = ipcc_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('IPCC reference region')  +
  ylab('Fraction')  +
  labs(fill = '')  +
  scale_fill_manual(values = c("SQR dry > SQR wet" = colset_RdBu_5[1], 
                               "SQR wet > SQR dry" = colset_RdBu_5[5],
                               "SQR dry = SQR wet" = "gray90"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "bar_ipcc_cumulative_fraction_SQR_ratio_spi.png"), 
       width = 16, height = 8)


#### land use
landuse <- evap_stats_masks[,.(stats_area = sum(area)),.(ratio_std_quantile_brk, land_cover_short_class)]
landuse <- landuse[complete.cases(landuse)]
landuse[, land_area:= sum(stats_area), .( land_cover_short_class)]
landuse[, land_fraction:= stats_area/land_area]

ggplot(landuse) +
  geom_bar(aes(x = land_cover_short_class, y = land_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('Land use')  +
  ylab('Fraction')  +
  labs(fill = '')  +
  scale_fill_manual(values = c("SQR dry > SQR wet" = colset_RdBu_5[5], 
                               "SQR wet > SQR dry" = colset_RdBu_5[1],
                               "SQR dry = SQR wet" = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "bar_landuse_cumulative_fraction_SQR_ratio_spi.png"), 
       width = 16, height = 8)
