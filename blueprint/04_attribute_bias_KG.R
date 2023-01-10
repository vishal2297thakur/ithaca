source('source/blueprint.R')
source('source/masks.R')
source('source/graphics.R')

## Read data 
prec_era5_kenya <- readRDS(paste0(path_save_blueprint, "prec_era5.rds"))
prec_stats_mean <- readRDS(paste0(path_save_blueprint, "prec_stats_mean.rds"))
prec_stats_low_bias <- readRDS(paste0(path_save_blueprint, "prec_stats_low_bias.rds"))

fname_shape <- list.files(path = masks_dir_KG_beck, full.names = TRUE, pattern = "climate_beck_level3.shp")
shape_mask <- st_read(paste0(fname_shape[1]))
shape_mask <- st_make_valid(shape_mask)

## Crop mask
shape_mask_crop <- st_crop(shape_mask, study_area)
shape_mask_raster <- rasterize(shape_mask_crop, prec_era5_kenya[[1]])
shape_mask_df <- shape_mask_raster %>% as.data.frame(xy = TRUE, long = TRUE, na.rm = TRUE)
shape_mask_df <- subset(shape_mask_df, select = c('x', 'y', 'value'))
colnames(shape_mask_df) <- c('lon', 'lat', 'KG_class')

## Merge
prec_stats_mean <- merge(prec_stats_mean, shape_mask_df, by = c('lon', 'lat'), all.x = T)
prec_stats_mean <- prec_stats_mean[complete.cases(prec_stats_mean)]
prec_stats_low_bias <- merge(prec_stats_low_bias, shape_mask_df, by = c('lon', 'lat'), all.x = T)
prec_stats_low_bias <- prec_stats_low_bias[complete.cases(prec_stats_low_bias)]

## Analysis
KG_class_size <- prec_stats_mean[, .N, KG_class]
KG_class_size_low_bias <- prec_stats_low_bias[, .N, KG_class]
KG_class_size_low_bias[, fraction := round(N/KG_class_size$N, 2)]

## Quick validation
ggplot() +
  geom_raster(data = shape_mask_df, aes(lon, lat, fill = KG_class)) +
  geom_point(data = prec_stats_low_bias,  aes(lon, lat)) +
  scale_fill_manual(values = colset_mid_qual) +
  theme_light()

ggplot(prec_stats_mean) +
  geom_bar(aes(x = KG_class)) + 
  theme_light()

#High-low precipitation
#Seasonal
#Elevation
#Vegetation

