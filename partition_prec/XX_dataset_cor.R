source('source/partition_prec.R')
source('source/graphics.R')
source('source/geo_functions.R')

install.packages(c("corrr", "statip"))
library(corrr)
library(statip)

prec_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_datasets.rds"))
prec_stats <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_ensemble_stats.rds"))
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))

data_for_distance <- dcast(prec_datasets, lon + lat ~ dataset, value.var = 'prec_mean')
data_for_distance <- data_for_distance[complete.cases(data_for_distance)]

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

hellinger(dummy$cmorph, dummy$cpc)

dummy[mahalanobis > 1000]


data_for_cor <- dcast(prec_datasets, lon + lat ~ dataset, value.var = 'prec_mean')
colnames(data_for_cor)[c(7, 8, 12, 16, 17)] <- c("cru", "em-earth", "gpm", "doe", "ncar")

data_for_cor <- merge(prec_mask[, .(lon, lat, prec_quant_dataset_agreement)], data_for_cor, by = c('lon', 'lat'))

data_for_cor <- data_for_cor[complete.cases(data_for_cor)]


data_for_cor <- data_for_cor[prec_quant_dataset_agreement != 'low']
cor_mat <- correlate(data_for_cor[, -(1:3)])

network_plot(cor_mat, min_cor = 0.7, legend = 'range')

