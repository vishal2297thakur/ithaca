source('source/blueprint.R')
source('source/graphics.R')

install.packages("gtools")
library(gtools)

## Read data 
prec_stats <- readRDS(paste0(path_save_blueprint, "ensemble_prec_stats.rds"))
prec_mask_mean <- readRDS(paste0(path_save_blueprint, "prec_mask_mean.rds"))
prec_mask_mean_month <- readRDS(paste0(path_save_blueprint, "prec_mask_mean_month.rds"))

## Analysis
prec_quant_size <- prec_mask_mean[, .(total_grid_cells = .N), prec_class]
prec_quant_size_bias <-  prec_mask_mean[, .N, .(prec_class, quant_cv)]
prec_quant_size_bias <- merge(prec_quant_size, prec_quant_size_bias, by = 'prec_class')
prec_quant_size_bias[, fraction := round(N/total_grid_cells, 2)]
prec_quant_size_bias <- prec_quant_size_bias[order(prec_class, quant_cv), ]

prec_quant_size_bias_cum <- prec_quant_size_bias[, .(grid_cell_cum = cumsum(N), quant_cv), .(prec_class)]
prec_quant_size_bias_cum[, fraction_cv_bias := round(grid_cell_cum / sum(grid_cell_cum), 2), quant_cv]

prec_prec_class <- prec_mask_mean[, .(sum_prec_class = sum(mean), 
                                      sum_prec_class_upper = sum(upper_bound_prec),
                                      sum_prec_class_lower = sum(lower_bound_prec)), 
                                  .(prec_class, quant_cv)]
prec_prec_class <- prec_prec_class[order(prec_class, quant_cv), ]
prec_prec_class_cum <- prec_prec_class[, .(cumsum_prec_class = cumsum(sum_prec_class), 
                                           cumsum_prec_class_upper = cumsum(sum_prec_class_upper), 
                                           cumsum_prec_class_lower = cumsum(sum_prec_class_lower), 
                                           quant_cv), .(prec_class)]
prec_prec_class_cum[, fraction_cv_bias := round(cumsum_prec_class / sum(cumsum_prec_class), 2), quant_cv]

## Quick validation
ggplot() +
  geom_raster(data = prec_mask_mean, aes(lon, lat, fill = prec_class)) +
  geom_point(data = prec_mask_mean[quant_cv == '0-0.2', .(lon, lat)],  aes(lon, lat)) +
  scale_fill_manual(values = colset_mid[8:4]) +
  labs(fill = 'Precipitation')  +
  theme_light()

ggplot() +
  geom_raster(data = prec_mask_mean, aes(lon, lat, fill = prec_class)) +
  geom_point(data = prec_mask_mean[quant_cv == '0-0.2' | quant_cv == '0.2-0.4' | quant_cv == '0.4-0.6', .(lon, lat)],  aes(lon, lat)) +
  scale_fill_manual(values = colset_mid[8:4]) +
  labs(fill = 'Precipitation')  +
  theme_light()

ggplot() +
  geom_raster(data = prec_mask_mean_month, aes(lon, lat, fill = cut(mean, 12))) +
  geom_point(data =  unique(prec_mask_mean_month[quant_cv == '0-0.2', .(lon, lat, month)]),  aes(lon, lat), size = 1) +
  scale_fill_manual(values = colset_mid) +
  labs(fill = 'Precipitation')  +
  facet_wrap(~month) +
  theme_light()

ggplot(prec_mask_mean) +
  geom_bar(aes(x = prec_class)) + 
  theme_light()

ggplot(prec_quant_size_bias) +
  geom_bar(aes(x = prec_class, y = fraction), stat="identity") +
  facet_wrap(~quant_cv) +
  xlab('Precipitation Quantile')  +
  theme_light()

## Plot
ggplot(prec_prec_class_cum) +
  geom_bar(aes(x = quant_cv, y = fraction_cv_bias, fill = prec_class), stat="identity") +
  xlab('CV Quantile')  +
  ylab('Fraction of Total Precipitation')  +
  labs(fill = 'Precipitation')  +
  scale_fill_manual(values = colset_mid_qual[1:5]) +
  theme_light()

ggplot(prec_prec_class_cum) +
  geom_bar(aes(x = quant_cv, y = cumsum_prec_class, fill = prec_class), stat="identity") +
  xlab('CV quantile')  +
  ylab('Total Precipitation')  +
  labs(fill = 'Precipitation')  +
  scale_fill_manual(values = colset_mid_qual[1:5], labels = c('low', 'below average',  'average', 'above average', 'high')) +
  theme_light()

ggplot(prec_prec_class_cum) +
  geom_bar(aes(x = quant_cv, y = cumsum_prec_class_lower, fill = prec_class), stat="identity") +
  xlab('CV quantile')  +
  ylab('Total Precipitation')  +
  labs(fill = 'Precipitation')  +
  scale_fill_manual(values = colset_mid_qual[1:5], labels = c('low', 'below average',  'average', 'above average', 'high')) +
  theme_light()

