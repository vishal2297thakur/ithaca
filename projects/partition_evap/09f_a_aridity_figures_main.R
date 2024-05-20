# Figure of standardized quartile range for various standaridized indices ---- 
## Biome and Land use 
# Aridity: precipitation 
## a and b precipitation
## c and d soil moisture
# Temperature: 
## e and f 
# ENSO
## g and h

source('source/partition_evap.R')
source('source/geo_functions.R')
source('source/graphics.R')

## Data ----
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))

## Precipitation ----

### Data ----
evap_stats_dry <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_dry_spi.rds"))
evap_stats_wet <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_wet_spi.rds"))

### Figure prep  ----

evap_stats <- merge(evap_stats_dry, evap_stats_wet, by = c("lon", "lat"), suffixes = c(".dry", ".wet"))

evap_stats[, ratio_std_quantile := std_quant_range.dry/std_quant_range.wet]
evap_stats[, ratio_std_quantile_brk := cut(ratio_std_quantile, breaks = c(0, 0.9, 1.11,500))]

evap_stats[, ratio_std_quantile_brk := factor(ratio_std_quantile_brk, levels = c("(0,0.9]", "(0.9,1.11]", "(1.11,500]"),
                                              labels = c("wet > dry             ",
                                                                  "dry = wet             ",
                                                                  "dry > wet             "), 
                                              ordered = T),]
evap_stats_masks <- merge(evap_stats, evap_mask, all.x = T, by = c("lon", "lat"))
grid_cell_area <- unique(evap_stats[, .(lon, lat)]) %>% grid_area() # m2
evap_stats_masks <- grid_cell_area[evap_stats_masks, on = .(lon, lat)]

### Land cover ----
landuse <- evap_stats_masks[,.(stats_area = sum(area)),.(ratio_std_quantile_brk, land_cover_short_class)]
landuse <- landuse[complete.cases(landuse)]
landuse[, land_area:= sum(stats_area), .( land_cover_short_class)]
landuse[, land_fraction:= stats_area/land_area]

fig_a_spi_landuse <- ggplot(landuse) +
  geom_bar(aes(x = land_cover_short_class, y = land_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('Land cover type')  +
  ylab('Area Fraction')  +
  labs(fill = "SPI SQR")  +
  scale_fill_manual(values = c("dry > wet             " = colset_RdBu_5[1], 
                                        "wet > dry             " = colset_RdBu_5[5],
                                        "dry = wet             " = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

### Biome ----
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

fig_b_spi_biome  <- ggplot(biome_stats) +
  geom_bar(aes(x = biome_short_class, y = biome_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('Biome')  +
  ylab('Area fraction')  +
  labs(fill = "SPI SQR")  +
  scale_fill_manual(values = c("dry > wet             " = colset_RdBu_5[1], 
                                        "wet > dry             " = colset_RdBu_5[5],
                                        "dry = wet             " = "gray90"))+
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

## Soil moisture ----
### Data ----
evap_stats_dry <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_dry_ssi_era5-land.rds"))
evap_stats_wet <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_wet_ssi_era5-land.rds"))

### Figure prep  ----

evap_stats <- merge(evap_stats_dry, evap_stats_wet, by = c("lon", "lat"), suffixes = c(".dry", ".wet"))

evap_stats[, ratio_std_quantile := std_quant_range.dry/std_quant_range.wet]
evap_stats[, ratio_std_quantile_brk := cut(ratio_std_quantile, breaks = c(0, 0.9, 1.11,500))]

evap_stats[, ratio_std_quantile_brk := factor(ratio_std_quantile_brk, levels = c("(0,0.9]", "(0.9,1.11]", "(1.11,500]"),
                                              labels = c("wet > dry             ",
                                                                  "dry = wet             ",
                                                                  "dry > wet             "), 
                                              ordered = T),]
evap_stats_masks <- merge(evap_stats, evap_mask, all.x = T, by = c("lon", "lat"))
grid_cell_area <- unique(evap_stats[, .(lon, lat)]) %>% grid_area() # m2
evap_stats_masks <- grid_cell_area[evap_stats_masks, on = .(lon, lat)]

### Land cover ----
landuse <- evap_stats_masks[,.(stats_area = sum(area)),.(ratio_std_quantile_brk, land_cover_short_class)]
landuse <- landuse[complete.cases(landuse)]
landuse[, land_area:= sum(stats_area), .( land_cover_short_class)]
landuse[, land_fraction:= stats_area/land_area]

fig_c_ssi_landuse <- ggplot(landuse) +
  geom_bar(aes(x = land_cover_short_class, y = land_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('Land cover type')  +
  ylab('Area Fraction')  +
  labs(fill = "SSI SQR")  +
  scale_fill_manual(values = c("dry > wet             " = colset_RdBu_5[1], 
                                        "wet > dry             " = colset_RdBu_5[5],
                                        "dry = wet             " = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

### Biome ----
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

fig_d_ssi_biome  <- ggplot(biome_stats) +
  geom_bar(aes(x = biome_short_class, y = biome_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('Biome')  +
  ylab('Area fraction')  +
  labs(fill = "SSI SQR")  +
  scale_fill_manual(values = c("dry > wet             " = colset_RdBu_5[1], 
                                        "wet > dry             " = colset_RdBu_5[5],
                                        "dry = wet             " = "gray90"))+
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


## Temperature ----
### Data ----

evap_stats_cold <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_cold_sti_era5-land.rds"))
evap_stats_warm <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_warm_sti_era5-land.rds"))

### Figure prep ----
evap_stats <- merge(evap_stats_cold, evap_stats_warm, by = c("lon", "lat"), suffixes = c(".cold", ".warm"))

evap_stats[, ratio_std_quantile := std_quant_range.warm/std_quant_range.cold]
evap_stats[, ratio_std_quantile_brk := cut(ratio_std_quantile, breaks = c(0, 0.9, 1.11, 500))]

evap_stats[, ratio_std_quantile_brk := factor(ratio_std_quantile_brk, levels = c("(0,0.9]", "(0.9,1.11]", "(1.11,500]"),
                                              labels = c("cold > warm       ",
                                                                  "warm = cold        ",
                                                                  "warm > cold        "), 
                                              ordered = T),]
evap_stats_masks <- merge(evap_stats, evap_mask, all.x = T, by = c("lon", "lat"))
grid_cell_area <- unique(evap_stats[, .(lon, lat)]) %>% grid_area() # m2
evap_stats_masks <- grid_cell_area[evap_stats_masks, on = .(lon, lat)]

### Land cover ---- 

landuse <- evap_stats_masks[,.(stats_area = sum(area)),.(ratio_std_quantile_brk, land_cover_short_class)]
landuse <- landuse[complete.cases(landuse)]
landuse[, land_area:= sum(stats_area), .( land_cover_short_class)]
landuse[, land_fraction:= stats_area/land_area]

fig_e_sti_landuse <- ggplot(landuse) +
  geom_bar(aes(x = land_cover_short_class, y = land_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('Land cover type')  +
  ylab('Area Fraction')  +
  labs(fill = 'STI SQR')  +
  scale_fill_manual(values = c("cold > warm       " = colset_RdBu_5[5], 
                                        "warm > cold        " = colset_RdBu_5[1],
                                        "warm = cold        " = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

### Biomes ----
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

fig_f_sti_biome <- ggplot(biome_stats) +
  geom_bar(aes(x = biome_short_class, y = biome_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('Biome')  +
  ylab('Area fraction')  +
  labs(fill = 'STI SQR')  +
  scale_fill_manual(values = c("cold > warm       " = colset_RdBu_5[5], 
                                        "warm > cold        " = colset_RdBu_5[1],
                                        "warm = cold        " = "gray90"))+
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

## ENSO ----
### Data ----

evap_stats_el_nino <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_ENSO_el_nino.rds"))
evap_stats_la_nina <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats_ENSO_la_nina.rds"))

### Figure prep ----
evap_stats <- merge(evap_stats_el_nino, evap_stats_la_nina, by = c("lon", "lat"), suffixes = c(".el_nino", ".la_nina"))
evap_stats[, ratio_std_quantile := std_quant_range.el_nino/std_quant_range.la_nina]
evap_stats[, ratio_std_quantile_brk := cut(ratio_std_quantile, breaks = c(0, 0.9, 1.11, 500))]

evap_stats[, ratio_std_quantile_brk := factor(ratio_std_quantile_brk, levels = c("(0,0.9]", "(0.9,1.11]", "(1.11,500]"),
                                              labels = c("la nina > el nino ",
                                                                  "la nina = el nino ","el nino > la nina  "), 
                                              ordered = T),]
evap_stats_masks <- merge(evap_stats, evap_mask, all.x = T, by = c("lon", "lat"))
grid_cell_area <- unique(evap_stats[, .(lon, lat)]) %>% grid_area() # m2
evap_stats_masks <- grid_cell_area[evap_stats_masks, on = .(lon, lat)]

### Land use ----

landuse <- evap_stats_masks[,.(stats_area = sum(area)),.(ratio_std_quantile_brk, land_cover_short_class)]
landuse <- landuse[complete.cases(landuse)]
landuse[, land_area:= sum(stats_area), .(land_cover_short_class)]
landuse[, land_fraction:= stats_area/land_area]

fig_g_ninos_landuse <- ggplot(landuse) +
  geom_bar(aes(x = land_cover_short_class, y = land_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('Land cover type')  +
  ylab('Area Fraction')  +
  labs(fill = 'ENSO SQR')  +
  scale_fill_manual(values = c("la nina > el nino " = colset_RdBu_5[5], 
                                        "el nino > la nina  " = colset_RdBu_5[1],
                                        "la nina = el nino " = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


### Biomes ----
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


fig_h_ninos_biome <- ggplot(biome_stats) +
  geom_bar(aes(x = biome_short_class, y = biome_fraction, fill = ratio_std_quantile_brk), stat = "identity") +
  xlab('Biome')  +
  ylab('Area Fraction')  +
  labs(fill = 'ENSO SQR')  +
  scale_fill_manual(values = c("la nina > el nino " = colset_RdBu_5[5], 
                                        "el nino > la nina  " = colset_RdBu_5[1],
                                        "la nina = el nino " = "gray90"))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


## composite figures ----

ab <- ggarrange(fig_a_spi_landuse, fig_b_spi_biome, common.legend = T,
                legend = "right", labels = c("a", "b"), align = "hv", widths = c(0.75, 1.1))

cd <- ggarrange(fig_c_ssi_landuse, fig_d_ssi_biome,  common.legend = T,
                legend = "right", labels = c("c", "d"), align = "hv", widths = c(0.75, 1.1))

ef <- ggarrange(fig_e_sti_landuse, fig_f_sti_biome, common.legend = T,
                legend = "right", labels = c("e", "f"), align = "hv", widths = c(0.75, 1.1))

gh <- ggarrange(fig_g_ninos_landuse, fig_h_ninos_biome, common.legend = T,
                legend = "right", labels = c("g", "h"), align = "hv", widths = c(0.75, 1.1))


ggarrange(ab, cd, ef, gh, align = "hv", nrow = 4)

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "fig_3_main_SQR_aridity_indices.png"), 
       width = 8, height = 12)

