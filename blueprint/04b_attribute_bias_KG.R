source('source/blueprint.R')
source('source/masks.R')
source('source/graphics.R')

## Read data 
prec_mask_mean <- readRDS(paste0(path_save_blueprint, "prec_mask_mean.rds"))
prec_mask_mean_month <- readRDS(paste0(path_save_blueprint, "prec_mask_mean_month.rds"))

## Analysis
KG_class_size <- prec_mask_mean[, .(total_grid_cells = .N), KG_class]
KG_class_size_low_bias <- prec_mask_mean[quant_cv == '0-0.2', .N, KG_class]
KG_class_size_low_bias[, fraction := round(N/KG_class_size$total_grid_cells, 2)]
KG_class_size_bias <-  prec_mask_mean[, .N, .(KG_class, quant_cv)]
KG_class_size_bias <- merge(KG_class_size, KG_class_size_bias, by = 'KG_class')
KG_class_size_bias[, fraction := round(N/total_grid_cells, 2)]
KG_class_size_bias <- KG_class_size_bias[order(KG_class, quant_cv), ]

KG_class_size_bias_cum <- KG_class_size_bias[, .(grid_cell_cum = cumsum(N), quant_cv), .(KG_class)]
KG_class_size_bias_cum[, fraction_cv_bias := round(grid_cell_cum / sum(grid_cell_cum), 2), quant_cv]

## Quick validation
ggplot() +
  geom_raster(data = prec_mask_mean, aes(lon, lat, fill = KG_class)) +
  geom_point(data = prec_mask_mean[dataset_agreement == 'high', .(lon, lat)],  aes(lon, lat)) +
  scale_fill_manual(values = colset_mid_qual[3:5]) +
  labs(fill = 'Precipitation')  +
  theme_light() +
  coord_sf(default_crs = sf::st_crs(3395)) + #Mercator
  scale_x_continuous(labels = function(x) paste0(x, '\u00B0'), expand = c(0, 0)) +
  scale_y_continuous(labels = function(x) paste0(x, '\u00B0'), expand = c(0, 0)) +
  theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

ggplot(prec_mask_mean) +
  geom_bar(aes(x = KG_class)) + 
  theme_light()

ggplot(KG_class_size_bias) +
  geom_bar(aes(x = KG_class, y = fraction), stat="identity") +
  facet_wrap(~quant_cv) +
  xlab('KG Class')  +
  theme_light()


## Plot
ggplot(KG_class_size_bias_cum) +
  geom_bar(aes(x = quant_cv, y = fraction_cv_bias, fill = KG_class), stat="identity") +
  xlab('CV Quantile')  +
  scale_fill_manual(values = colset_mid_qual[3:5]) +
  theme_light()
