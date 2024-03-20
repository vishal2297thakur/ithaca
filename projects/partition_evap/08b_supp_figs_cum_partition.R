# Supplementary figure: Cumulative sum and dataset agreement - Needs editing

source('source/partition_evap.R')
source('source/graphics.R')
source('source/geo_functions.R')

## Data 
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
evap_stats <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats.rds"))
load(paste0(PATH_SAVE_PARTITION_EVAP, "partition_evap.Rdata"))

## Variables
evap_mask <- evap_mask[evap_stats[, .(lon, lat, evap_mean = ens_mean_mean)], on = .(lon, lat)]
evap_mask <- evap_mask[complete.cases(evap_mask)]
levels(evap_mask$rel_dataset_agreement) <- c("High", "Above average", "Average", "Below average", "Low")

## Analysis
### Land cover
land_cover_short_class_evap <- evap_mask[, .(lon, lat, evap_mean,
                                           land_cover_short_class, rel_dataset_agreement)]
land_cover_short_class_evap <- land_cover_short_class_evap[complete.cases(land_cover_short_class_evap)]
dummie_area <- grid_area(land_cover_short_class_evap[, .(lon, lat)])
land_cover_short_class_evap <- merge(land_cover_short_class_evap, dummie_area,
                                   by = c("lon", "lat"))
land_cover_short_class_evap[, rel_area := sum(area),
                          by = .(land_cover_short_class, rel_dataset_agreement)
][, wevap_mean := evap_mean * area / rel_area
][, evap_wavg := sum(wevap_mean),
  by = .(land_cover_short_class, rel_dataset_agreement)
][, evap_dif := evap_mean - evap_wavg
][, evap_n := nrow(.SD),
  by = .(land_cover_short_class, rel_dataset_agreement)
][, evap_sd := sqrt(sum(evap_dif^2)/(evap_n -1)),
  by = .(land_cover_short_class, rel_dataset_agreement)]

land_cover_short_class_evap <- land_cover_short_class_evap[, .(evap_wavg, evap_sd,
                                                           evap_median = median(evap_mean),
                                                           evap_q25 = quantile(evap_mean, 0.25),
                                                           evap_q75 = quantile(evap_mean, 0.75)), 
                                                       .(land_cover_short_class, rel_dataset_agreement)]
land_cover_short_class_evap <- unique(land_cover_short_class_evap)

land_cover_agreement_cum <- land_cover_agreement[, .(cumsum_land_cover = cumsum(evap_sum), 
                                                 rel_dataset_agreement), .(land_cover_short_class)]
land_cover_agreement_cum[, fraction_bias := cumsum_land_cover  / sum(cumsum_land_cover), rel_dataset_agreement]
land_cover_agreement_cum <- land_cover_agreement_cum[complete.cases(land_cover_agreement_cum)]

### Biomes
biome_short_class_evap <- evap_mask[, .(lon, lat, evap_mean, biome_short_class, rel_dataset_agreement)]
biome_short_class_evap <- biome_short_class_evap[complete.cases(biome_short_class_evap)]
dummie_area <- grid_area(biome_short_class_evap[, .(lon, lat)])
biome_short_class_evap <- merge(biome_short_class_evap, dummie_area,
                                by = c("lon", "lat"))
biome_short_class_evap[, rel_area := sum(area),
                       by = .(biome_short_class, rel_dataset_agreement)
][, wevap_mean := evap_mean*area/rel_area
][, evap_wavg := sum(wevap_mean),
  by = .(biome_short_class, rel_dataset_agreement)
][, evap_dif := evap_mean - evap_wavg
][, evap_n := nrow(.SD),
  by = .(biome_short_class, rel_dataset_agreement)
][, evap_sd := sqrt(sum(evap_dif^2)/(evap_n -1)),
  by = .(biome_short_class, rel_dataset_agreement)]

biome_short_class_evap <- biome_short_class_evap[, .(evap_wavg, evap_sd,
                                                     evap_median = median(evap_mean),
                                                     evap_q25 = quantile(evap_mean, 0.25),
                                                     evap_q75 = quantile(evap_mean, 0.75)), 
                                                 .(biome_short_class, rel_dataset_agreement)]
biome_short_class_evap <- unique(biome_short_class_evap)

biome_agreement_cum <- biome_agreement[, .(cumsum_biome = cumsum(evap_sum), 
                                           rel_dataset_agreement), .(biome_short_class)]
biome_agreement_cum[, fraction_bias := cumsum_biome  / sum(cumsum_biome), rel_dataset_agreement]
biome_agreement_cum <- biome_agreement_cum[complete.cases(biome_agreement_cum)]

### Elevation
elev_class_evap <- evap_mask[, .(lon, lat, evap_mean, elev_class, rel_dataset_agreement)]
elev_class_evap <- elev_class_evap[complete.cases(elev_class_evap)]
dummie_area <- grid_area(elev_class_evap[, .(lon, lat)])
elev_class_evap <- merge(elev_class_evap, dummie_area,
                         by = c("lon", "lat"))
elev_class_evap[, rel_area := sum(area),
                by = .(elev_class, rel_dataset_agreement)
][, wevap_mean := evap_mean*area/rel_area
][, evap_wavg := sum(wevap_mean),
  by = .(elev_class, rel_dataset_agreement)
][, evap_dif := evap_mean - evap_wavg
][, evap_n := nrow(.SD),
  by = .(elev_class, rel_dataset_agreement)
][, evap_sd := sqrt(sum(evap_dif^2)/(evap_n -1)),
  by = .(elev_class, rel_dataset_agreement)]

elev_class_evap <- elev_class_evap[, .(evap_wavg, evap_sd,
                                       evap_median = median(evap_mean),
                                       evap_q25 = quantile(evap_mean, 0.25),
                                       evap_q75 = quantile(evap_mean, 0.75)), 
                                   .(elev_class, rel_dataset_agreement)]
elev_class_evap <- unique(elev_class_evap)

elev_agreement_cum <- elevation_agreement[, .(cumsum_elev = cumsum(evap_sum), 
                                              rel_dataset_agreement), .(elev_class)]
elev_agreement_cum[, fraction_bias := cumsum_elev  / sum(cumsum_elev), rel_dataset_agreement]
elev_agreement_cum <- elev_agreement_cum[complete.cases(elev_agreement_cum)]

### evaporation Quantiles
evap_quant_class_evap <- evap_mask[, .(lon, lat, evap_mean, evap_quant, rel_dataset_agreement)]
evap_quant_class_evap <- evap_quant_class_evap[complete.cases(evap_quant_class_evap)]
dummie_area <- grid_area(evap_quant_class_evap[, .(lon, lat)])
evap_quant_class_evap <- merge(evap_quant_class_evap, dummie_area,
                               by = c("lon", "lat"))
evap_quant_class_evap[, rel_area := sum(area),
                      by = .(evap_quant, rel_dataset_agreement)
][, wevap_mean := evap_mean*area/rel_area
][, evap_wavg := sum(wevap_mean),
  by = .(evap_quant, rel_dataset_agreement)
][, evap_dif := evap_mean - evap_wavg
][, evap_n := nrow(.SD),
  by = .(evap_quant, rel_dataset_agreement)
][, evap_sd := sqrt(sum(evap_dif^2)/(evap_n -1)),
  by = .(evap_quant, rel_dataset_agreement)]

evap_quant_class_evap <- evap_quant_class_evap[, .(evap_wavg, evap_sd,
                                                   evap_median = median(evap_mean),
                                                   evap_q25 = quantile(evap_mean, 0.25),
                                                   evap_q75 = quantile(evap_mean, 0.75)), 
                                               .(evap_quant, rel_dataset_agreement)]
evap_quant_class_evap <- unique(evap_quant_class_evap)

evap_agreement_cum <- evap_quant_agreement[, .(cumsum_evap = cumsum(evap_sum), 
                                               rel_dataset_agreement), .(evap_quant)]
evap_agreement_cum[, fraction_bias := cumsum_evap  / sum(cumsum_evap), rel_dataset_agreement]
evap_agreement_cum <- evap_agreement_cum[complete.cases(evap_agreement_cum)]


## Figures

#land_cover
ggplot(land_cover_short_class_evap) + 
  geom_errorbar(aes(x = rel_dataset_agreement, ymin = evap_wavg - evap_sd, ymax = evap_wavg + evap_sd, col = rel_dataset_agreement), 
                position = "dodge", width = 0.25) +
  geom_point(aes(x = rel_dataset_agreement, y = evap_wavg, col = rel_dataset_agreement)) +
  facet_wrap(~ land_cover_short_class, scales = 'free') +
  labs(col = 'Dataset agreement')  +
  xlab('Dataset agreement')  +
  ylab('Mean annual evaporation')  +
  scale_color_manual(values = colset_agreement) +
  theme_light() +
  theme(strip.background = element_rect(fill = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/land_cover_median_evap.png"), width = 8, height = 6)

ggplot(land_cover_agreement_cum) +
  geom_bar(aes(x = rel_dataset_agreement, y = fraction_bias , fill = land_cover_short_class), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('evaporation fraction')  +
  labs(fill = 'Land cover class')  +
  scale_fill_manual(values = colset_land_cover_short) +
  theme_light()
ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/land_cover_agreement_cum.png"), width = 8, height = 6)


#biome
ggplot(biome_short_class_evap) + 
  geom_errorbar(aes(x = rel_dataset_agreement, ymin = evap_wavg - evap_sd, ymax = evap_wavg + evap_sd, col = rel_dataset_agreement), 
                position = "dodge", width = 0.25) +
  geom_point(aes(x = rel_dataset_agreement, y = evap_wavg, col = rel_dataset_agreement)) +
  facet_wrap(~ biome_short_class, scales = 'free') +
  labs(col = 'Dataset agreement')  +
  xlab('Dataset agreement')  +
  ylab('Mean annual evaporation')  +
  scale_color_manual(values = colset_agreement) +
  theme_light() +
  theme(strip.background = element_rect(fill = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_PARTITION_evap_FIGURES, "supplement/biome_median_evap.png"), width = 8, height = 6)

ggplot(biome_agreement_cum) +
  geom_bar(aes(x = rel_dataset_agreement, y = fraction_bias , fill = biome_short_class), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('evaporation fraction')  +
  labs(fill = 'Biome category')  +
  scale_fill_manual(values = colset_biome_short[c(2, 8, 10, 9, 6, 1, 7, 5, 4, 3)]) +
  theme_light()
ggsave(paste0(PATH_SAVE_PARTITION_evap_FIGURES, "supplement/biome_agreement_cum.png"), width = 8, height = 6)

#elevation
ggplot(elev_class_evap) + 
  geom_errorbar(aes(x = rel_dataset_agreement, ymin = evap_wavg - evap_sd, ymax = evap_wavg + evap_sd, col = rel_dataset_agreement), 
                position = "dodge", width = 0.25) +
  geom_point(aes(x = rel_dataset_agreement, y = evap_wavg, col = rel_dataset_agreement)) +
  facet_wrap(~ elev_class, scales = 'free') +
  labs(col = 'Dataset agreement')  +
  xlab('Dataset agreement')  +
  ylab('Mean annual evaporation')  +
  scale_color_manual(values = colset_agreement) +
  theme_light() +
  theme(strip.background = element_rect(fill = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/elev_median_evap.png"), width = 8, height = 6)

ggplot(elev_agreement_cum) +
  geom_bar(aes(x = rel_dataset_agreement, y = fraction_bias , fill = elev_class), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('evaporation fraction')  +
  labs(fill = 'Elevation zone')  +
  scale_fill_manual(values = rev(c("dark red", colset_RdBu_5))) +
  theme_light()
ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/elev_agreement_cum.png"), width = 8, height = 6)

#evaporation_quantiles
ggplot(evap_quant_class_evap) + 
  geom_errorbar(aes(x = rel_dataset_agreement, ymin = evap_wavg - evap_sd, ymax = evap_wavg + evap_sd, col = rel_dataset_agreement), 
                position = "dodge", width = 0.25) +
  geom_point(aes(x = rel_dataset_agreement, y = evap_wavg, col = rel_dataset_agreement)) +
  facet_wrap(~ evap_quant, scales = 'free') +
  labs(col = 'Dataset agreement')  +
  xlab('Dataset agreement')  +
  ylab('Mean annual evaporation')  +
  scale_color_manual(values = colset_agreement) +
  theme_light() +
  theme(strip.background = element_rect(fill = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/evap_median_evap.png"), width = 8, height = 6)

ggplot(evap_agreement_cum) +
  geom_bar(aes(x = rel_dataset_agreement, y = fraction_bias , fill = evap_quant), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('evaporation fraction')  +
  labs(fill = 'evap. quantile')  +
  scale_fill_manual(values = colset_evap_quant) +
  theme_light()
ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/evap_agreement_cum.png"), width = 8, height = 6)
