### Add categorical classes to each variable 

source('source/blueprint.R')
source('source/geo_functions.R')
source('source/graphics.R')
source('source/masks.R')

install.packages("gtools")
library(gtools)

## Read data 
prec_era5_kenya <- readRDS(paste0(path_save_blueprint, "prec_era5.rds"))
prec_stats_mean <- readRDS(paste0(path_save_blueprint, "prec_stats_mean.rds"))
prec_stats_mean_month <- readRDS(paste0(path_save_blueprint, "prec_stats_mean_month.rds"))

## Masks
# Precipitation
prec_stats_mean[, quant := ordered(quantcut(mean, 5), labels = c('0-0.2', '0.2-0.4', '0.4-0.6', '0.6-0.8', '0.8-1.00'))]

# Bias: Coefficient of Variation
prec_stats_mean[, bias_cv := 'high']
prec_stats_mean[cv <= mid_cv_bias, bias_cv := 'medium']
prec_stats_mean[cv <= low_cv_bias, bias_cv := 'low']

# Koppen-Geiger
fname_shape <- list.files(path = masks_dir_KG_beck, full.names = TRUE, pattern = "climate_beck_level3.shp")
shape_mask <- st_read(paste0(fname_shape[1]))
shape_mask <- st_make_valid(shape_mask)

shape_mask_crop <- st_crop(shape_mask, study_area)
shape_mask_raster <- rasterize(shape_mask_crop, prec_era5_kenya[[1]])
shape_mask_df <- shape_mask_raster %>% as.data.frame(xy = TRUE, long = TRUE, na.rm = TRUE)
shape_mask_df <- subset(shape_mask_df, select = c('x', 'y', 'value'))
colnames(shape_mask_df) <- c('lon', 'lat', 'KG_class')

prec_stats_mean <- merge(prec_stats_mean, shape_mask_df, by = c('lon', 'lat'), all.x = T)
prec_stats_mean <- prec_stats_mean[complete.cases(prec_stats_mean)]

# Elevation

# Land use

# Vegetation

# Save for further use
prec_stats_mean_month <- merge(prec_stats_mean_month, prec_stats_mean[, .(lon, lat, quant, bias_cv, KG_class)], by = c("lon", "lat"))

saveRDS(prec_stats_mean, paste0(path_save_blueprint, "prec_mask_mean.rds"))
saveRDS(prec_stats_mean_month, paste0(path_save_blueprint, "prec_mask_mean_month.rds"))


## Plot results
to_plot <- prec_stats_low_bias
p00 <- ggplot() +
  geom_raster(data = to_plot, aes(x = lon, y = lat, fill = "")) +
  geom_point(data = ghcn_stations_kenya, aes(x = lon, y = lat), col = 'dark red', size = 3) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(to_plot$lon), max(to_plot$lon)), 
                  ylim = c(min(to_plot$lat), max(to_plot$lat))) +  
  labs(x = "", y = "", fill = 'Low bias') +
  scale_x_continuous(expand = c(0.015, 0.015)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.grid = element_line(color = "grey")) 

y_labs <- ggplot_build(p00)$layout$panel_params[[1]]$y$get_labels()
x_labs <- ggplot_build(p00)$layout$panel_params[[1]]$x$get_labels()
p01 <- p00 + scale_x_continuous(expand = c(0.015, 0.015), labels = paste0(x_labs, "\u00b0")) +
  scale_y_continuous(expand = c(0.0125, 0.0125),  labels = paste0(y_labs, "\u00b0"))
p01

to_plot <- cv_values_n
ggplot(to_plot, aes(y = size, x = cv)) +
  geom_point() +
  geom_line() +
  labs(y = "Number of grid cells", x = "CV") +
  theme_light() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.grid = element_line(color = "grey")) 


