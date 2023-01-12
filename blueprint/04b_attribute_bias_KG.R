source('source/blueprint.R')
source('source/masks.R')
source('source/graphics.R')

## Read data 
prec_mask_mean <- readRDS(paste0(path_save_blueprint, "prec_mask_mean.rds"))
prec_mask_mean_month <- readRDS(paste0(path_save_blueprint, "prec_mask_mean_month.rds"))

## Analysis
KG_class_size <- prec_mask_mean[, .N, KG_class]
KG_class_size_low_bias <- prec_mask_mean[bias_cv == 'low', .N, KG_class]
KG_class_size_low_bias[, fraction := round(N/KG_class_size$N, 2)]

## Quick validation
ggplot() +
  geom_raster(data = prec_mask_mean, aes(lon, lat, fill = KG_class)) +
  geom_point(data = prec_mask_mean[bias_cv == 'low', .(lon, lat)],  aes(lon, lat)) +
  scale_fill_manual(values = colset_mid_qual[3:5]) +
  labs(fill = 'Precipitation')  +
  theme_light()

ggplot(prec_mask_mean) +
  geom_bar(aes(x = KG_class)) + 
  theme_light()

#High-low precipitation
#Seasonal
#Elevation
#Vegetation

