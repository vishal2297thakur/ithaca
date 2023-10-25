# No global dataset so not applicable for evaporation


# Plots the datasets that are closests/furthest to the ensemble mean

source('source/partition_evap.R')
source('source/graphics.R')
source('source/geo_functions.R')

library(ggrepel)
library(tidyverse)

#Data
datasets_vol <- data.table(read.csv(paste0(PATH_SAVE_PARTITION_EVAP_TABLES, 
                                                "partition_KG_datasets_global.csv")))[, 2:8]

## Variables
datasets_vol_matrix <- t(as.matrix(datasets_vol[, 2:7])) 
colnames(datasets_vol_matrix) <- datasets_vol$Dataset
datasets_vol <- melt(datasets_vol)
colnames(datasets_vol) <- c('dataset', 'climate', 'evap')

## Analysis
KG_means <- datasets_vol[, .(evap_mean = mean(evap)), climate]
datasets_mean_ratio <- datasets_vol_matrix / KG_means$evap_mean

datasets_mean_ratio <- as_tibble(datasets_mean_ratio) %>%
  mutate(climate = factor(rownames(datasets_mean_ratio)))  %>%
  relocate(climate)

datasets_mean_ratio <- datasets_mean_ratio %>% pivot_longer(-climate,
             names_to="dataset",
             values_to="evap") 

datasets_mean_ratio <- data.table(datasets_mean_ratio)

datasets_mean_ratio[, evap_diff := evap - 1]
datasets_mean_ratio[, abs_evap_diff := abs(evap_diff)]

datasets_mean_ratio <- datasets_mean_ratio[order(-rank(climate), abs_evap_diff)]
dummy_1 <- datasets_mean_ratio[datasets_mean_ratio[, .I[dataset == head(dataset, 3)], by = climate]$V1]
dummy_1[, class := factor("mean")]
datasets_mean_ratio <- datasets_mean_ratio[order(-rank(climate), evap_diff)]
dummy_2 <- datasets_mean_ratio[datasets_mean_ratio[, .I[dataset == head(dataset, 2)], by = climate]$V1]
dummy_2[, class := factor("underestimate")]
dummy_3 <- datasets_mean_ratio[datasets_mean_ratio[, .I[dataset == tail(dataset, 2)], by = climate]$V1]
dummy_3[, class := factor("overestimate")]

## Figures
to_plot <- rbind(dummy_1, dummy_2, dummy_3)[, c(1, 2, 3, 6)]

ggplot(to_plot, aes(climate, evap)) +
  geom_hline(yintercept = 1, col = 'grey50') +
  geom_point() +
  geom_line(aes(group = climate)) +
  scale_fill_manual(values = colset_RdBu_5[c(3, 4, 2)]) + 
  scale_color_manual(values = c('white', 'black', 'white')) + 
  geom_label_repel(aes(label = dataset, fill = class),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  scale_x_discrete(name = "Climate") +
  scale_y_continuous(name = "Ratio to the ensemble mean") +
  guides(fill = "none") +
  theme_bw() +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12)) 

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "dataset_performance.png"), width = 8, height = 8)

 
