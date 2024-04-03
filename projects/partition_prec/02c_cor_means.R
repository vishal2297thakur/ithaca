# Estimates the mean cross-correlation coefficient at each grid cell

source('source/partition_prec.R')
source('source/geo_functions.R')

prec_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_datasets.rds"))
prec_stats <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_ensemble_stats.rds"))

prec_datasets <- prec_datasets[order(lon, lat), ]
prec_datasets[, grid_id := .GRP, .(lon, lat)]

grid_ids <- unique(prec_datasets[, .(grid_id = factor(grid_id), lon, lat)])
prec_datasets_tab <- dcast(prec_datasets[, .(grid_id, year, dataset, prec)], 
                           grid_id + year ~ dataset)
prec_datasets_list <- lapply(split(prec_datasets_tab[, -2], 
                           by = 'grid_id'), cor, use = 'pairwise.complete.obs') 
mean_cor <- sapply(prec_datasets_list, mean, na.rm = T)
mean_cor <- merge(grid_ids, 
      data.table(grid_id = factor(names(mean_cor)), dataset_cor = mean_cor),
      by = 'grid_id')
hist(mean_cor$dataset_cor)

prec_stats <- merge(prec_stats, mean_cor[, .(lon, lat, ens_mean_cor = round(dataset_cor, 2))], 
              by = c('lon', 'lat'))
saveRDS(prec_stats, paste0(PATH_SAVE_PARTITION_PREC, "prec_ensemble_stats.rds"))



prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))


to_plot <- merge(prec_stats, prec_mask)

ggplot(to_plot) +
  geom_boxplot(aes(x = prec_quant_dataset_agreement, y = ens_mean_cor), outlier.color = 'white') +
  facet_wrap(~biome_short_class) +
  theme_linedraw()

ggplot(to_plot) +
  geom_boxplot(aes(x = prec_quant_dataset_agreement, y = ens_mean_cor), outlier.color = 'white') +
  facet_wrap(~prec_quant) +
  theme_linedraw()
