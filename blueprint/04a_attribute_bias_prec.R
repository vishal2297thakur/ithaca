install.packages("gtools")
library(gtools)

source('source/blueprint.R')
source('source/graphics.R')

## Read data 
prec_stats <- readRDS(paste0(path_save_blueprint, "ensemble_prec_stats.rds"))
prec_mask_mean <- readRDS(paste0(path_save_blueprint, "prec_mask_mean.rds"))
prec_mask_mean_month <- readRDS(paste0(path_save_blueprint, "prec_mask_mean_month.rds"))

## Extra mask
prec_mask_mean[, abs_dataset_agreement := ordered(1, labels = "very high")]
prec_mask_mean[std_quant_range > 0.1 & std_quant_range < 0.2, abs_dataset_agreement := ordered(2, labels = "high")]
prec_mask_mean[std_quant_range > 0.2 & std_quant_range < 0.4, abs_dataset_agreement := ordered(3, labels = "above average")]
prec_mask_mean[std_quant_range > 0.4 & std_quant_range < 0.6, abs_dataset_agreement := ordered(4, labels = "average")]
prec_mask_mean[std_quant_range > 0.6 & std_quant_range < 0.8, abs_dataset_agreement := ordered(5, labels = "below average")]
prec_mask_mean[std_quant_range > 0.8 & std_quant_range < 1, abs_dataset_agreement := ordered(6, labels = "low")]
prec_mask_mean[std_quant_range > 1, abs_dataset_agreement := ordered(7, labels = "very low")]

prec_mask_mean_month[, abs_dataset_agreement := ordered(1, labels = "very high")]
prec_mask_mean_month[std_quant_range > 0.1 & std_quant_range < 0.2, abs_dataset_agreement := ordered(2, labels = "high")]
prec_mask_mean_month[std_quant_range > 0.2 & std_quant_range < 0.4, abs_dataset_agreement := ordered(3, labels = "above average")]
prec_mask_mean_month[std_quant_range > 0.4 & std_quant_range < 0.6, abs_dataset_agreement := ordered(4, labels = "average")]
prec_mask_mean_month[std_quant_range > 0.6 & std_quant_range < 0.8, abs_dataset_agreement := ordered(5, labels = "below average")]
prec_mask_mean_month[std_quant_range > 0.8 & std_quant_range < 1, abs_dataset_agreement := ordered(6, labels = "low")]
prec_mask_mean_month[std_quant_range > 1, abs_dataset_agreement := ordered(7, labels = "very low")]


## Analysis | TODO: Estimate the partition by different monthly dataset agreement classes
prec_quant_size <- prec_mask_mean[, .(total_grid_cells = .N), prec_class]
prec_quant_size_bias <-  prec_mask_mean[, .N, .(prec_class, abs_dataset_agreement)]
prec_quant_size_bias <- merge(prec_quant_size, prec_quant_size_bias, by = 'prec_class')
prec_quant_size_bias[, fraction := round(N / total_grid_cells, 2)]
prec_quant_size_bias <- prec_quant_size_bias[order(prec_class, abs_dataset_agreement), ]

prec_quant_size_bias_cum <- prec_quant_size_bias[, .(grid_cell_cum = cumsum(N), abs_dataset_agreement), .(prec_class)]
prec_quant_size_bias_cum[, fraction_cv_bias := round(grid_cell_cum / sum(grid_cell_cum), 2), abs_dataset_agreement]

prec_prec_class <- prec_mask_mean[, .(sum_prec_class = sum(ens_mean), 
                                      sum_prec_class_upper = sum(ens_q75),
                                      sum_prec_class_lower = sum(ens_q25)), 
                                  .(prec_class, abs_dataset_agreement)]
prec_prec_class <- prec_prec_class[order(prec_class, abs_dataset_agreement), ]
prec_prec_class_cum <- prec_prec_class[, .(cumsum_prec_class = cumsum(sum_prec_class), 
                                           cumsum_prec_class_upper = cumsum(sum_prec_class_upper), 
                                           cumsum_prec_class_lower = cumsum(sum_prec_class_lower), 
                                           abs_dataset_agreement), .(prec_class)]
prec_prec_class_cum[, fraction_cv_bias := round(cumsum_prec_class / sum(cumsum_prec_class), 2), abs_dataset_agreement]

## Quick validation
ggplot() +
  geom_raster(data = prec_mask_mean, aes(lon, lat, fill = prec_class)) +
  geom_point(data = prec_mask_mean[abs_dataset_agreement == 'average' | abs_dataset_agreement == 'above average' , .(lon, lat)],  aes(lon, lat)) +
  scale_fill_manual(values = colset_mid[8:4]) +
  labs(fill = 'Precipitation')  +
  theme_light()

ggplot() +
  geom_raster(data = prec_mask_mean_month, aes(lon, lat, fill = cut(ens_mean, 12))) +
  geom_point(data =  unique(prec_mask_mean_month[abs_dataset_agreement == 'above average', .(lon, lat, month)]),  aes(lon, lat), size = 1) +
  scale_fill_manual(values = colset_mid) +
  labs(fill = 'Precipitation')  +
  facet_wrap(~month) +
  theme_light()

ggplot(prec_mask_mean) +
  geom_bar(aes(x = prec_class)) + 
  theme_light()

ggplot(prec_quant_size_bias) +
  geom_bar(aes(x = prec_class, y = fraction), stat = "identity") +
  facet_wrap(~abs_dataset_agreement) +
  xlab('Precipitation Type')  +
  theme_light()

## Plot
ggplot(prec_prec_class_cum) +
  geom_bar(aes(x = abs_dataset_agreement, y = fraction_cv_bias, fill = prec_class), stat="identity") +
  xlab('Dataset agreement')  +
  ylab('Fraction of Total Precipitation')  +
  labs(fill = 'Precipitation')  +
  scale_fill_manual(values = colset_mid_qual[1:5]) +
  theme_light()

ggplot(prec_prec_class) +
  geom_bar(aes(x = abs_dataset_agreement, y = sum_prec_class, fill = prec_class), stat="identity") +
  xlab('Dataset agreement')  +
  ylab('Total Precipitation')  +
  labs(fill = 'Precipitation')  +
  scale_fill_manual(values = colset_mid_qual[1:5], labels = c('low', 'below average',  'average', 'above average', 'high')) +
  theme_light()

ggplot(prec_prec_class) +
  geom_bar(aes(x = abs_dataset_agreement, y = sum_prec_class_lower, fill = prec_class), stat="identity") +
  xlab('CV quantile')  +
  ylab('Total Precipitation')  +
  labs(fill = 'Precipitation')  +
  scale_fill_manual(values = colset_mid_qual[1:5], labels = c('low', 'below average',  'average', 'above average', 'high')) +
  theme_light()

