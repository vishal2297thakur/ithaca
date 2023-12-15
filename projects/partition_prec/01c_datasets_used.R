# Create subsets of datasets to be used for dataset agreement and mean estimates

source('source/partition_prec.R')
source('source/geo_functions.R')

library(openair)
library(tidyverse)
prec_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_datasets.rds"))
prec_datasets_annual <- prec_datasets[, .(prec = mean(prec, na.rm = TRUE)), .(year, dataset)]

taylor_obs <- prec_datasets_annual[, .(prec_mean = mean(prec, na.rm = TRUE)), .(year)]
data_for_taylor <- merge(taylor_obs, prec_datasets_annual, by = "year")
data_for_taylor[, dataset := factor(dataset)]
setnames(data_for_taylor, c('prec_mean', 'prec'), c('obs', 'mod')) 
data_for_taylor[, bias := mod - obs]
representativess <- data_for_taylor[, .(mean_bias = mean(bias)), dataset]
representativess[order(mean_bias), ]
TaylorDiagram(data_for_taylor, group = "dataset") 
ggplot() +
  geom_line(data = data_for_taylor, aes(x = year, y = mod, col = factor(dataset), group = dataset)) +
  geom_point(data = taylor_obs, aes(x = factor(year), y = prec_mean), 
            col = 'black') +
  theme_linedraw()

data_for_cor <- dcast(data_for_taylor, year ~ dataset, value.var = 'mod')
cor_matrix <- cor(data_for_cor[, -1], use = 'pairwise.complete.obs')

outliers <- c('ncep-doe', 'ncep-ncar', 'cmap', 'chirps', 'cpc', 'persiann', 'cmorph')
outliers <- c('ncep-doe', 'chirps', 'cpc', 'persiann', 'cmorph')

taylor_obs <- prec_datasets_annual[!dataset %in% outliers, .(prec_mean = mean(prec, na.rm = TRUE)), .(year)]
data_for_taylor <- merge(taylor_obs, prec_datasets_annual, by = "year")
setnames(data_for_taylor, c('prec_mean', 'prec'), c('obs', 'mod')) 
data_for_taylor[, bias := mod - obs]
representativess <- data_for_taylor[, .(mean_bias = mean(bias)), dataset]
representativess[order(mean_bias), ]
ggplot() +
  geom_line(data = data_for_taylor, aes(x = year, y = mod, col = factor(dataset), group = dataset)) +
  geom_point(data = taylor_obs, aes(x = factor(year), y = prec_mean), 
             col = 'black') +
  theme_linedraw()


mergers <- c('gpm-imerg', 'mswep')

taylor_obs <- prec_datasets_annual[!dataset %in% c(outliers, mergers), .(prec_mean = mean(prec, na.rm = TRUE)), .(year)]
data_for_taylor <- merge(taylor_obs, prec_datasets_annual, by = "year")
setnames(data_for_taylor, c('prec_mean', 'prec'), c('obs', 'mod')) 
data_for_taylor[, bias := mod - obs]
representativess <- data_for_taylor[, .(mean_bias = mean(bias)), dataset]
representativess[order(mean_bias), ]
ggplot() +
  geom_line(data = data_for_taylor[!dataset %in% outliers], aes(x = year, y = mod, col = factor(dataset), group = dataset)) +
  geom_point(data = taylor_obs, aes(x = factor(year), y = prec_mean), 
             col = 'black') +
  theme_linedraw()

prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))

#low agreement
prec_mask[lon == -68.375 & lat == -55.375]

taylor_obs <- prec_datasets[!dataset %in% c(outliers, mergers) & lon == -68.375 & lat == -55.375, .(prec_mean = mean(prec, na.rm = TRUE)), .(year)]
data_for_taylor <- merge(taylor_obs, prec_datasets[lon == -68.375 & lat == -55.375], by = "year")
setnames(data_for_taylor, c('prec_mean', 'prec'), c('obs', 'mod')) 
data_for_taylor[, bias := mod - obs]
representativess <- data_for_taylor[, .(mean_bias = mean(bias)), dataset]
representativess[order(mean_bias), ]
ggplot() +
  geom_line(data = data_for_taylor[!dataset %in% outliers], aes(x = year, y = mod, col = factor(dataset), group = dataset)) +
  geom_point(data = taylor_obs, aes(x = factor(year), y = prec_mean), 
             col = 'black') +
  theme_linedraw()

#high agreement 
prec_mask[lon == 38.375 & lat == 55.375]

taylor_obs <- prec_datasets[!dataset %in% c(outliers, mergers) & lon == 38.375 & lat == 55.375, .(prec_mean = mean(prec, na.rm = TRUE)), .(year)]
data_for_taylor <- merge(taylor_obs, prec_datasets[lon == 38.375 & lat == 55.375], by = "year")
setnames(data_for_taylor, c('prec_mean', 'prec'), c('obs', 'mod')) 
data_for_taylor[, bias := mod - obs]
representativess <- data_for_taylor[, .(mean_bias = mean(bias)), dataset]
representativess[order(mean_bias), ]
ggplot() +
  geom_line(data = data_for_taylor, aes(x = year, y = mod, col = factor(dataset), group = dataset)) +
  geom_point(data = taylor_obs, aes(x = factor(year), y = prec_mean), 
             col = 'black') +
  theme_linedraw()



#Data
datasets_vol <- data.table(read.csv(paste0(PATH_SAVE_PARTITION_PREC_TABLES, 
                                           "partition_KG_datasets_global.csv")))[, 2:8]

## Variables
datasets_vol_matrix <- t(as.matrix(datasets_vol[, 2:7])) 
colnames(datasets_vol_matrix) <- datasets_vol$Dataset
datasets_vol <- melt(datasets_vol)
colnames(datasets_vol) <- c('dataset', 'climate', 'prec')

## Analysis
KG_means <- datasets_vol[!dataset %in% c(outliers, mergers), .(prec_mean = mean(prec)), climate]
datasets_mean_ratio <- datasets_vol_matrix / KG_means$prec_mean

datasets_mean_ratio <- as_tibble(datasets_mean_ratio) %>%
  mutate(climate = factor(rownames(datasets_mean_ratio)))  %>%
  relocate(climate)

datasets_mean_ratio <- datasets_mean_ratio %>% pivot_longer(-climate,
                                                            names_to="dataset",
                                                            values_to="prec") 
datasets_mean_ratio <- data.table(datasets_mean_ratio)


