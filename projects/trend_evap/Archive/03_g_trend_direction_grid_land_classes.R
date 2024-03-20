# Cumulative trend direction per mask item  ----

source('source/evap_trend.R')
source('source/graphics.R')

## Data ----
evap_trend_volume <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_evap_area.rds"))

## Analysis ----
evap_trend_volume[significant_theil_sen*theil_sen_slope > 0, trend_score := 2  ]
evap_trend_volume[significant_theil_sen*theil_sen_slope < 0, trend_score := -2  ]
evap_trend_volume[significant_theil_sen == FALSE & theil_sen_slope > 0, trend_score:= 1  ]
evap_trend_volume[significant_theil_sen == FALSE & theil_sen_slope <= 0, trend_score := -1  ]

evap_trend_volume[trend_score == 2, trend_direction := "positive significant"]
evap_trend_volume[trend_score == -2, trend_direction := "negative significant"]
evap_trend_volume[trend_score == 1, trend_direction := "positive"]
evap_trend_volume[trend_score == -1, trend_direction := "negative"]

evap_trend_volume[,trend_direction := ordered(trend_direction, levels =  c("negative significant", "negative","positive","positive significant")),]

### Biome types ----
biome_trends <- evap_trend_volume[,.(trend_area = sum(area)),.(trend_direction,biome_class,dataset)]
biome_trends <- biome_trends[complete.cases(biome_trends)]
biome_trends[, biome_area:= sum(trend_area), .(biome_class, dataset)]
biome_trends[, biome_fraction:= trend_area/biome_area]

ggplot(biome_trends) +
  geom_bar(aes(x = dataset, y = biome_fraction, fill = trend_direction), stat = "identity") +
  xlab('Dataset')  +
  ylab('Area fraction')  +
  labs(fill = 'Trend direction')  +
  scale_fill_manual(values = c("negative significant" = "darkblue", "positive significant" = "darkred", "negative" = "royalblue1", "positive" = "lightcoral"))+
  theme_light() +
  facet_wrap(~biome_class, ncol = 1)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "all_evap_trend_biome_fraction_products.png"), 
       width = 8, height = 12)

### ipcc ref regions ----
ipcc_trends <- evap_trend_volume[,.(trend_area = sum(area)),.(trend_direction,IPCC_ref_region,dataset)]
ipcc_trends <- ipcc_trends[complete.cases(ipcc_trends)]
ipcc_trends[, ipcc_area:= sum(trend_area), .(IPCC_ref_region, dataset)]
ipcc_trends[, ipcc_fraction:= trend_area/ipcc_area]
ipcc_trends[grepl("O", as.character(IPCC_ref_region)) == TRUE, ocean := "yes"]
ipcc_trends[IPCC_ref_region %in% c("RAR", "BOB", "ARS"), ocean := "yes"]


ggplot(ipcc_trends[is.na(ocean),]) +
  geom_bar(aes(x = dataset, y = ipcc_fraction, fill = trend_direction), stat = "identity") +
  xlab('Dataset')  +
  ylab('Fraction')  +
  labs(fill = 'Trend direction')  +
  scale_fill_manual(values = c("negative significant" = "darkblue", "positive significant" = "darkred", "negative" = "royalblue1", "positive" = "lightcoral"))+
  theme_light() +
  facet_wrap(~IPCC_ref_region, ncol = 3)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "all_evap_trend_ipcc_fraction_products.png"), 
       width = 12, height = 16)

### land use ----
land_trends <- evap_trend_volume[,.(trend_area = sum(area)),.(trend_direction, land_cover_short_class, dataset)]
land_trends <- land_trends[complete.cases(land_cover_short_class)]
land_trends[, land_area:= sum(trend_area), .(land_cover_short_class, dataset)]
land_trends[, land_fraction:= trend_area/land_area]

ggplot(land_trends) +
  geom_bar(aes(x = dataset, y = land_fraction, fill = trend_direction), stat = "identity") +
  xlab('Dataset')  +
  ylab('Fraction')  +
  labs(fill = 'Trend direction')  +
  scale_fill_manual(values = c("negative significant" = "darkblue", "positive significant" = "darkred", "negative" = "royalblue1", "positive" = "lightcoral"))+
  theme_light() +
  facet_wrap(~land_cover_short_class, ncol = 1)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "all_evap_trend_land_cover_fraction_products.png"), 
       width = 8, height = 12)


### elevation ----
elev_trends <- evap_trend_volume[,.(trend_area = sum(area)),.(trend_direction, elev_class, dataset)]
elev_trends <- elev_trends[complete.cases(elev_class)]
elev_trends[, elev_area:= sum(trend_area), .(elev_class, dataset)]
elev_trends[, elev_fraction:= trend_area/elev_area]

ggplot(elev_trends) +
  geom_bar(aes(x = dataset, y = elev_fraction, fill = trend_direction), stat = "identity") +
  xlab('Dataset')  +
  ylab('Fraction')  +
  labs(fill = 'Trend direction')  +
  scale_fill_manual(values = c("negative significant" = "darkblue", "positive significant" = "darkred", "negative" = "royalblue1", "positive" = "lightcoral"))+
  theme_light() +
  facet_wrap(~elev_class, ncol = 1)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "all_evap_trend_elevation_fraction_products.png"), 
       width = 8, height = 12)


### KG 3 ----
KG_class_3_trends <- evap_trend_volume[,.(trend_area = sum(area)),.(trend_direction, KG_class_3, dataset)]
KG_class_3_trends <- KG_class_3_trends[complete.cases(KG_class_3_trends)]
KG_class_3_trends[, KG_class_3_area:= sum(trend_area), .(KG_class_3, dataset)]
KG_class_3_trends[, KG_class_3_fraction:= trend_area/KG_class_3_area]

ggplot(KG_class_3_trends) +
  geom_bar(aes(x = dataset, y = KG_class_3_fraction, fill = trend_direction), stat = "identity") +
  xlab('Dataset')  +
  ylab('Fraction')  +
  labs(fill = 'Trend direction')  +
  scale_fill_manual(values = c("negative significant" = "darkblue", "positive significant" = "darkred", "negative" = "royalblue1", "positive" = "lightcoral"))+
  theme_light() +
  facet_wrap(~KG_class_3, ncol = 3)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "all_evap_trend_KG_class_3_fraction_products.png"), 
       width = 12, height = 16)
