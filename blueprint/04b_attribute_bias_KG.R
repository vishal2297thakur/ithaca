source('source/blueprint.R')
source('source/masks.R')
source('source/graphics.R')

## Read data 
prec_mask_mean <- readRDS(paste0(path_save_blueprint, "prec_mask_mean.rds"))
prec_mask_mean_month <- readRDS(paste0(path_save_blueprint, "prec_mask_mean_month.rds"))

## Variables
needed_for_cumsum <- expand.grid(prec_mask_mean[, unique(dataset_agreement)], prec_mask_mean[, unique(KG_class)])
colnames(needed_for_cumsum) <- c("dataset_agreement", 'KG_class')

## Analysis
KG_class_prec_bias <-  prec_mask_mean[, .(prec_sum = round(sum(ens_mean), 0)), .(KG_class, dataset_agreement)]
KG_class_prec_bias <-  merge(KG_class_prec_bias, needed_for_cumsum, by = c('KG_class', "dataset_agreement"), all.y = TRUE)
KG_class_prec_bias[is.na(prec_sum), prec_sum := 0]
KG_class_prec_bias <-  KG_class_prec_bias[,  prec_fraction := round(prec_sum / sum(prec_sum), 2)]
KG_class_prec_bias <- KG_class_prec_bias[order(KG_class, dataset_agreement), ]

KG_class_prec_bias_cum <- KG_class_prec_bias[, .(prec_cumsum = cumsum(prec_sum), dataset_agreement), .(KG_class)]
KG_class_prec_bias_cum[, fraction_prec_bias := prec_cumsum / sum(prec_cumsum), dataset_agreement]

KG_class_size <- prec_mask_mean[, .(total_grid_cells = .N), KG_class]
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
  coord_sf(default_crs = sf::st_crs(3395)) + #Mercator
  labs(fill = 'Precipitation')  +
  theme_light()

ggplot(prec_mask_mean) +
  geom_bar(aes(x = KG_class)) + 
  theme_light()

ggplot(KG_class_size_bias) +
  geom_bar(aes(x = KG_class, y = fraction), stat="identity") +
  facet_wrap(~quant_cv) +
  xlab('KG Class')  +
  theme_light()

## Plot
# Main: Partition (%) of total precipitation per climatology for different levels of dataset agreement 
ggplot(KG_class_prec_bias_cum) +
  geom_bar(aes(x = dataset_agreement, y = fraction_prec_bias , fill = KG_class), stat="identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('Precipitation fraction')  +
  labs(fill = 'KG class')  +
  scale_fill_manual(values = colset_mid_qual[3:5]) +
  theme_light()

# Alternative: Partition (%) of climatology area for different quantiles of coef. of variation
ggplot(KG_class_size_bias_cum) +
  geom_bar(aes(x = quant_cv, y = fraction_cv_bias, fill = KG_class), stat="identity") +
  xlab('CV Quantile')  +
  scale_fill_manual(values = colset_mid_qual[3:5]) +
  theme_light()


