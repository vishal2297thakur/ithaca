source('source/blueprint.R')
source('source/graphics.R')

## Read data 
prec_mask_mean <- readRDS(paste0(path_save_blueprint, "prec_mask_mean.rds"))
prec_mask_mean_month <- readRDS(paste0(path_save_blueprint, "prec_mask_mean_month.rds"))

## Analysis
prec_quant_size <- prec_mask_mean[, .(total_grid_cells = .N), quant_mean]
prec_quant_size_low_bias <- prec_mask_mean[quant_cv == '0-0.2', .N, quant_mean]
prec_quant_size_low_bias[, fraction := round(N/prec_quant_size$N[1], 2)]
prec_quant_size_bias <-  prec_mask_mean[, .N, .(quant_mean, quant_cv)]
prec_quant_size_bias <- merge(prec_quant_size, prec_quant_size_bias, by = 'quant_mean')
prec_quant_size_bias[, fraction := round(N/total_grid_cells, 2)]
prec_quant_size_bias <- prec_quant_size_bias[order(quant_mean, quant_cv), ]

prec_quant_size_bias_cum <- prec_quant_size_bias[, .(grid_cell_cum = cumsum(N), quant_cv), .(quant_mean)]
prec_quant_size_bias_cum[, fraction_cv_bias := round(grid_cell_cum / sum(grid_cell_cum), 2), quant_cv]

prec_quant_mean <- prec_mask_mean[, .(sum_quant_mean = sum(mean)), .(quant_mean, quant_cv)]
prec_quant_mean <- prec_quant_mean[order(quant_mean, quant_cv), ]
prec_quant_mean_cum <- prec_quant_mean[, .(cumsum_quant_mean = cumsum(sum_quant_mean), quant_cv), .(quant_mean)]
prec_quant_mean_cum[, fraction_cv_bias := round(cumsum_quant_mean / sum(cumsum_quant_mean), 2), quant_cv]


## Quick validation
ggplot() +
  geom_raster(data = prec_mask_mean, aes(lon, lat, fill = quant_mean)) +
  geom_point(data = prec_mask_mean[quant_cv == '0-0.2', .(lon, lat)],  aes(lon, lat)) +
  scale_fill_manual(values = colset_mid[8:4], labels = c('low', 'below average',  'average', 'above average', 'high')) +
  labs(fill = 'Precipitation')  +
  theme_light()

ggplot() +
  geom_raster(data = prec_mask_mean, aes(lon, lat, fill = quant_mean)) +
  geom_point(data = prec_mask_mean[quant_cv == '0-0.2' | quant_cv == '0.2-0.4' | quant_cv == '0.2-0.6', .(lon, lat)],  aes(lon, lat)) +
  scale_fill_manual(values = colset_mid[8:4], labels = c('low', 'below average',  'average', 'above average', 'high')) +
  labs(fill = 'Precipitation')  +
  theme_light()

ggplot() +
  geom_raster(data = prec_mask_mean_month, aes(lon, lat, fill = cut(mean, 12))) +
  geom_point(data =  prec_mask_mean[quant_cv != '0.8-1.00', .(lon, lat)],  aes(lon, lat), size = 1) +
  scale_fill_manual(values = colset_mid) +
  labs(fill = 'Precipitation')  +
  facet_wrap(~month) +
  theme_light()

ggplot(prec_mask_mean) +
  geom_bar(aes(x = quant)) + 
  theme_light()

ggplot(prec_quant_size_bias) +
  geom_bar(aes(x = quant_mean, y = fraction), stat="identity") +
  facet_wrap(~quant_cv) +
  xlab('Precipitation Quantile')  +
  theme_light()

## Plot
ggplot(prec_quant_mean_cum) +
  geom_bar(aes(x = quant_cv, y = fraction_cv_bias, fill = quant_mean), stat="identity") +
  xlab('CV Quantile')  +
  ylab('Fraction of Total Precipitation')  +
  scale_fill_manual(values = colset_mid_qual[1:5]) +
  theme_light()

ggplot(prec_quant_mean_cum) +
  geom_bar(aes(x = quant_cv, y = cumsum_quant_mean, fill = quant_mean), stat="identity") +
  xlab('CV Quantile')  +
  ylab('Total Precipitation')  +
  scale_fill_manual(values = colset_mid_qual[1:5]) +
  theme_light()

