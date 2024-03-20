# Addition of categorical classes to each grid cell 

source('source/partition_prec.R')
source('source/geo_functions.R')
source('source/graphics.R')
source('source/mask_paths.R')

## Packages
library("gtools")

## Data 
masks_global <- readRDS(paste0(PATH_SAVE, "/misc/masks_global.rds"))
prec_stats <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_ensemble_stats.rds"))
prec_volume <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_volume_grid.rds"))

## Masks
### Precipitation
prec_stats[, prec_quant := ordered(quantcut(ens_mean_mean, 10), 
                                   labels = c('0-0.1', '0.1-0.2', '0.2-0.3', '0.3-0.4', '0.4-0.5',     #Caution: This is biased due to different grid size
                                              '0.5-0.6', '0.6-0.7', '0.7-0.8', '0.8-0.9', '0.9-1'))]

### Uncertainty - Dataset agreement
agreement_quant <-  c(0, 0.1, 0.3, 0.7, 0.9, 1) 

prec_stats[, rel_dataset_agreement := ordered(quantcut(std_quant_range, agreement_quant),
                                                          labels = c('high', 'above average', 'average', 'below average', 'low'))]

prec_stats[, prec_quant_dataset_agreement := ordered(quantcut(std_quant_range, agreement_quant),
                                                          labels = c('high', 'above average', 'average', 'below average', 'low')), 
           prec_quant]

prec_masks <- merge(masks_global, prec_stats[, .(lon, lat, prec_quant, rel_dataset_agreement, #Merges only complete cases
                               prec_quant_dataset_agreement)], by = c("lon", "lat"))

## Save data
saveRDS(prec_masks, paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))
