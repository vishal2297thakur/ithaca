source('source/blueprint.R')
source('source/graphics.R')

## Read data 
prec_mask_mean <- readRDS(paste0(path_save_blueprint, "prec_mask_mean.rds"))
prec_mask_mean_month <- readRDS(paste0(path_save_blueprint, "prec_mask_mean_month.rds"))
prec_stats_low_bias <- readRDS(paste0(path_save_blueprint, "prec_stats_low_bias.rds"))

## Analysis
prec_quant_size <- prec_mask_mean[, .N, quant]
prec_quant_size_low_bias <- prec_mask_mean[bias_cv == 'low', .N, quant]
prec_quant_size_low_bias[, fraction := round(N/prec_quant_size$N[1], 2)]

## Quick validation
ggplot() +
  geom_raster(data = prec_mask_mean, aes(lon, lat, fill = quant)) +
  geom_point(data = prec_mask_mean[bias_cv == 'low', .(lon, lat)],  aes(lon, lat)) +
  scale_fill_manual(values = colset_mid[8:4], labels = c('low', 'below average',  'average', 'above average', 'high')) +
  labs(fill = 'Precipitation')  +
  theme_light()

ggplot() +
  geom_raster(data = prec_mask_mean, aes(lon, lat, fill = quant)) +
  geom_point(data = prec_mask_mean[bias_cv == 'low' | bias_cv == 'medium', .(lon, lat)],  aes(lon, lat)) +
  scale_fill_manual(values = colset_mid[8:4], labels = c('low', 'below average',  'average', 'above average', 'high')) +
  labs(fill = 'Precipitation')  +
  theme_light()

ggplot() +
  geom_raster(data = prec_mask_mean_month, aes(lon, lat, fill = cut(mean, 12))) +
  geom_point(data =  prec_mask_mean[bias_cv == 'high', .(lon, lat)],  aes(lon, lat), size = 1) +
  scale_fill_manual(values = colset_mid) +
  labs(fill = 'Precipitation')  +
  facet_wrap(~month) +
  theme_light()

ggplot(prec_mask_mean) +
  geom_bar(aes(x = quant)) + 
  theme_light()


