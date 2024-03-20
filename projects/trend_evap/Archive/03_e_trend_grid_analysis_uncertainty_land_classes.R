# Create probability maps  ----
source('source/evap_trend.R')
source('source/graphics.R')

## Data ----
evap_trend_summary <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_probability_groups.rds"))
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))

## Analysis ----
evap_trend_masks <- merge(evap_trend_summary, evap_mask, all.x = T, by = c("lon", "lat"))


### Biome types ----

biome_trends <- evap_trend_masks[,.(trend_area = sum(area)),.(trend, biome_class)]
biome_trends <- biome_trends[complete.cases(biome_trends)]
biome_trends[, biome_area:= sum(trend_area), .(biome_class)]
biome_trends[, biome_fraction:= trend_area/biome_area]
biome_trends <- biome_trends[!(biome_class == "N/A")]

biome_trends[grepl("Tundra", biome_class) == TRUE, biome_short_class := "Tundra"]
biome_trends[grepl("Boreal Forests", biome_class) == TRUE, biome_short_class := "B. Forests"]
biome_trends[grepl("Dry Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Dry BL Forests"]
biome_trends[grepl("Moist Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Moist BL Forests"]
biome_trends[grepl("Subtropical Coniferous Forests", biome_class) == TRUE, biome_short_class := "T/S Coni. Forests"]
biome_trends[grepl("Temperate Conifer Forests", biome_class) == TRUE, biome_short_class := "T. Coni. Forests"]
biome_trends[grepl("Temperate Broadleaf & Mixed Forests", biome_class) == TRUE, biome_short_class := "T. BL Forests"]
biome_trends[grepl("Temperate Grasslands", biome_class) == TRUE, biome_short_class := "T. Grasslands"]
biome_trends[grepl("Subtropical Grasslands", biome_class) == TRUE, biome_short_class := "T/S Grasslands"]
biome_trends[grepl("Montane Grasslands", biome_class) == TRUE, biome_short_class := "M. Grasslands"]
biome_trends[grepl("Flooded", biome_class) == TRUE, biome_short_class := "Flooded"]
biome_trends[grepl("Mangroves", biome_class) == TRUE, biome_short_class := "Mangroves"]
biome_trends[grepl("Deserts", biome_class) == TRUE, biome_short_class := "Deserts"]
biome_trends[grepl("Mediterranean", biome_class) == TRUE, biome_short_class := "Mediterranean"]
biome_trends[, biome_short_class := factor(biome_short_class)]

biome_trends[, trend := factor(trend, levels = c("positive likely","positive probable", "no trend",
                                                 "negative probable",
                                                 "negative likely",
                                                 "uncertain"),
                               labels = c("positive likely","positive probable", "no trend",
                                                  "negative probable",
                                                  "negative likely",
                                                  "uncertain"), ordered = TRUE)]

ggplot(biome_trends) +
  geom_bar(aes(x = biome_short_class, y = biome_fraction, fill = trend), stat = "identity") +
  xlab('Biome')  +
  ylab('Area fraction')  +
  labs(fill = 'Trend')  +
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                                        "negative probable" = "steelblue1", 
                                        "positive likely" = "darkred", 
                                        "positive probable" = "darkorange", 
                                        "uncertain" = "deeppink1", 
                                        "no trend" = "gray80"))+
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "evap_trend_biome_uncertainty_probabilty.png"), 
       width = 12, height = 8)

### IPCC reference regions ----

ipcc_trends <- evap_trend_masks[,.(trend_area = sum(area)),.(trend, IPCC_ref_region)]
ipcc_trends <- ipcc_trends[complete.cases(ipcc_trends)]
ipcc_trends[, ipcc_area:= sum(trend_area), .(IPCC_ref_region)]
ipcc_trends[, ipcc_fraction:= trend_area/ipcc_area]
ipcc_trends <- ipcc_trends[!(is.na(IPCC_ref_region))]

ipcc_trends[, trend := factor(trend, levels = c("positive likely","positive probable", "no trend",
                                                 "negative probable",
                                                 "negative likely",
                                                 "uncertain"),
                               labels = c("positive likely","positive probable", "no trend",
                                          "negative probable",
                                          "negative likely",
                                          "uncertain"), ordered = TRUE)]


ggplot(ipcc_trends) +
  geom_bar(aes(x = IPCC_ref_region, y = ipcc_fraction, fill = trend), stat = "identity") +
  xlab('Dataset')  +
  ylab('Area fraction')  +
  labs(fill = 'Trend')  +
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "evap_trend_ipcc_ref_region_uncertainty_probabilty.png"), 
       width = 12, height = 8)
