# Addition of categorical classes to each grid cell 

source('source/partition_evap.R')
source('source/geo_functions.R')
source('source/graphics.R')
source('source/mask_paths.R')

## Packages
library("gtools")

## Data 
masks_global <- readRDS(paste0(PATH_SAVE, "/misc/masks_global.rds"))
evap_stats <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats.rds"))
evap_volume <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_volume_grid.rds"))

## Masks
### Evaporation
evap_stats[, evap_quant := ordered(quantcut(ens_mean_mean, 10), 
                                   labels = c('0-0.1', '0.1-0.2', '0.2-0.3', '0.3-0.4', '0.4-0.5',     #Caution: This is biased due to different grid size
                                              '0.5-0.6', '0.6-0.7', '0.7-0.8', '0.8-0.9', '0.9-1'))]

### Uncertainty - Dataset agreement
evap_stats[, quant_ens_cv := ordered(quantcut(ens_mean_cv, 5), 
                                         labels = c('0-0.2', '0.2-0.4', '0.4-0.6', '0.6-0.8', '0.8-1.00')), evap_quant]


## relative dataset agreement at quantile 0.1, 0.3, 0.7. 0.9
quant_thr_0_1 <- quantile(evap_stats$std_quant_range, c(0.1))
quant_thr_0_3 <- quantile(evap_stats$std_quant_range, c(0.3))
quant_thr_0_7 <- quantile(evap_stats$std_quant_range, c(0.7))
quant_thr_0_9 <- quantile(evap_stats$std_quant_range, c(0.9))

evap_stats[std_quant_range <= quant_thr_0_1, rel_dataset_agreement := ordered(1, labels = "high")] 
evap_stats[std_quant_range > quant_thr_0_1 & std_quant_range <= quant_thr_0_3, rel_dataset_agreement := ordered(3, labels = "above average")]
evap_stats[std_quant_range > quant_thr_0_3 & std_quant_range <= quant_thr_0_7, rel_dataset_agreement := ordered(4, labels = "average")]
evap_stats[std_quant_range > quant_thr_0_7 & std_quant_range <= quant_thr_0_9, rel_dataset_agreement := ordered(5, labels = "below average")]
evap_stats[std_quant_range > quant_thr_0_9, rel_dataset_agreement := ordered(7, labels = "low")]

evap_stats[std_quant_range <= 0.1, evap_quant_dataset_agreement := ordered(1, labels = "high")] 
evap_stats[std_quant_range > 0.1 & std_quant_range <= 0.3, evap_quant_dataset_agreement := ordered(3, labels = "above average")]
evap_stats[std_quant_range > 0.3 & std_quant_range <= 0.7, evap_quant_dataset_agreement := ordered(4, labels = "average")]
evap_stats[std_quant_range > 0.7 & std_quant_range <= 0.9, evap_quant_dataset_agreement := ordered(5, labels = "below average")]
evap_stats[std_quant_range > 0.9, evap_quant_dataset_agreement := ordered(7, labels = "low")]

evap_masks <- merge(masks_global, evap_stats[, .(lon, lat, evap_quant, rel_dataset_agreement, #Merges only complete cases
                               evap_quant_dataset_agreement)], by = c("lon", "lat"))

## Save data
saveRDS(evap_masks, paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))


## Code review
evap_masks <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))

ggplot(evap_masks)+
  geom_tile(aes(x = lon, y = lat, fill = rel_dataset_agreement))+
  theme_bw()

ggplot(evap_masks)+
  geom_tile(aes(x = lon, y = lat, fill = evap_quant_dataset_agreement))+
  theme_bw()
