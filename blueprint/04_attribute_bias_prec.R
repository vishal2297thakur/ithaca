source('source/blueprint.R')
source('source/graphics.R')

install.packages("gtools")
library(gtools)

## Read data 
prec_stats_mean <- readRDS(paste0(path_save_blueprint, "prec_stats_mean.rds"))
prec_stats_mean_month <- readRDS(paste0(path_save_blueprint, "prec_stats_mean_month.rds"))
prec_stats_low_bias <- readRDS(paste0(path_save_blueprint, "prec_stats_low_bias.rds"))

## Create mask
prec_stats_mean[, quant := ordered(quantcut(mean, 4), labels = c('0-0.25', '0.25-0.5', '0.5-0.75', '0.75-1.00'))]
prec_stats_mean_month[, quant := ordered(quantcut(mean, 4), labels = c('0-0.25', '0.25-0.5', '0.5-0.75', '0.75-1.00'))]
prec_stats_low_bias <- merge(prec_stats_low_bias, prec_stats_mean, by = c('lon', 'lat'), all.x = TRUE)

## Analysis
prec_quant_size_low_bias <- prec_stats_low_bias[, .N, quant]
prec_quant_size_low_bias[, fraction := round(N/prec_quant_size$N[1], 2)]

## Quick validation
ggplot() +
  geom_raster(data = prec_stats_mean, aes(lon, lat, fill = quant)) +
  geom_point(data = prec_stats_low_bias,  aes(lon, lat)) +
  scale_fill_manual(values = colset_mid_qual) +
  theme_light()

ggplot() +
  geom_raster(data = prec_stats_mean_month, aes(lon, lat, fill = cut(mean, 12))) +
  geom_point(data = prec_stats_low_bias,  aes(lon, lat), size = 1) +
  scale_fill_manual(values = colset_mid) +
  facet_wrap(~month) +
  theme_light()

ggplot(prec_stats_mean) +
  geom_bar(aes(x = quant)) + 
  theme_light()


