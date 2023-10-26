# Supplementary figure: KG partition (not used)

library(ggthemes)
library(scales)

source('source/partition_evap.R')
source('source/graphics.R')
source('source/geo_functions.R')

## Data 
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
evap_grid <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_grid.rds"))

## Variables
koppen_class <- merge(evap_mask[, .(lat, lon, rel_dataset_agreement, KG_class_1_name, land_cover_short_class)], evap_grid[, .(lon, lat, evap_volume_year)], by = c("lon", "lat"))

## Analysis
dataset_agreement_koppen <- koppen_class[, .N, .(rel_dataset_agreement, KG_class_1_name)]
dataset_agreement_koppen <- dataset_agreement_koppen[complete.cases(dataset_agreement_koppen)]
dataset_agreement_koppen <- dataset_agreement_koppen[order(rel_dataset_agreement, KG_class_1_name), ]
dataset_agreement_koppen[, koppen_sum := sum(N), KG_class_1_name]
dataset_agreement_koppen[, koppen_fraction := N/koppen_sum]

dataset_agreement_koppen_evap <- koppen_class[, .(evap_sum = sum(evap_volume_year)), .(land_cover_short_class, KG_class_1_name)]
dataset_agreement_koppen_evap <- dataset_agreement_koppen_evap[complete.cases(dataset_agreement_koppen_evap)]
dataset_agreement_koppen_evap <- dataset_agreement_koppen_evap[order(land_cover_short_class, KG_class_1_name), ]

## Figures Supplementary
levels(dataset_agreement_koppen$rel_dataset_agreement) <- c("High", "Above average", "Average",
                                             "Below average", "Low")

fig_koppen_partition_evap_volume <- ggplot(dataset_agreement_koppen_evap) +
  geom_bar(aes(x = reorder(KG_class_1_name, -(evap_sum)), y = evap_sum, fill = land_cover_short_class), stat = "identity") +
  #scale_y_continuous(label = axis_scientific) +
  xlab('Koppen-Geiger climate type')  +
  ylab(bquote('evaporation sum ['~km^3~year^-1~']'))  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = colset_land_cover_short) +
  theme_light()

dataset_agreement_koppen$KG_class_1_name <- factor(dataset_agreement_koppen$KG_class_1_name, 
                                                   levels = c("Tropical", "Temperate", "Continental", "Dry", "Polar"))

fig_koppen_partition_fraction <- ggplot(dataset_agreement_koppen) +
  geom_bar(aes(x = KG_class_1_name, y = koppen_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('Koppen-Geiger climate type')  +
  ylab('Fraction')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = colset_mid[c(10, 9, 6, 3, 1)]) +
  theme_light()

### Figure 1
gg_fig_KG <- ggarrange(fig_koppen_partition_evap_volume, fig_koppen_partition_fraction, 
                      labels = c('a', 'b'),
                      legend = 'right', 
                      nrow = 1, ncol = 2)

jpeg(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/KG_partition.png"), 
     width = 12, height = 6, units = 'in', res = 300)
gg_fig_KG
dev.off()

