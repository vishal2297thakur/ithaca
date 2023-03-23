source('source/partition_prec.R')
source('source/graphics.R')

install.packages(c("corrr", "statip"))
library(corrr)
library(statip)

prec_annual <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_global_annual_mean.rds"))
prec_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_datasets.rds"))
prec_stats <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_ensemble_stats.rds"))
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))

data_for_cor_temporal <- dcast(prec_annual, year ~ dataset, value.var = 'prec_mean')[, -1]
#data_for_distance_temporal <- data_for_distance_temporal[complete.cases(data_for_distance_temporal)]
data_for_cor_spatial <- dcast(prec_datasets, lon + lat ~ dataset, value.var = 'prec_mean')[, -(1:2)]
#data_for_distance_spatial <- data_for_distance[complete.cases(data_for_distance)]

#distances <- combn(names(data_for_distance_temporal), 2, function(x) 
#  hellinger(data_for_distance_spatial[[x[1]]], data_for_distance_temporal[[x[2]]]))

cor_mat <- correlate(data_for_cor_temporal)
cor_mat <- correlate(data_for_cor_temporal)
network_plot(cor_mat, min_cor = 0, legend = 'range')

# Dataset agreement and cor
data_for_cor <- merge(prec_mask[, .(lon, lat, prec_quant_dataset_agreement)], data_for_cor, by = c('lon', 'lat'))
data_for_cor <- data_for_cor[complete.cases(data_for_cor)]
data_for_cor <- data_for_cor[prec_quant_dataset_agreement != 'low']


## Mahalanobis

data_for_distance$mahalanobis <- mahalanobis(data_for_distance[, -(1:2)], 
                                             colMeans(data_for_distance[, -(1:2)]), 
                                             cov(data_for_distance[, -(1:2)]))

dummy <- merge(prec_stats, data_for_distance, by = c("lon", "lat"))
dummy <- merge(dummy, prec_mask[, .(lon, lat, rel_dataset_agreement)], by = c("lon", "lat"))

ggplot(dummy) +
  geom_point(aes(x = mahalanobis, y = std_quant_range, col = rel_dataset_agreement)) + 
  theme_bw()

ggplot(dummy) +
  geom_point(aes(x = std_quant_range, y = ens_mean_cv, col = rel_dataset_agreement)) + 
  theme_bw()