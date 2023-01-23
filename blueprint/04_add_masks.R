### Add categorical classes to each variable 

source('source/blueprint.R')
source('source/geo_functions.R')
source('source/graphics.R')
source('source/masks.R')

install.packages("gtools")
library(gtools)

## Read data 
prec_era5_kenya <- brick(paste0(path_save_blueprint, "era5_tp_mm_kenya_200006_201912_025_monthly.nc"))
prec_stats_mean <- readRDS(paste0(path_save_blueprint, "prec_stats_mean.rds"))
prec_stats_mean_month <- readRDS(paste0(path_save_blueprint, "prec_stats_mean_month.rds"))

## Masks
# Bias: Coefficient of Variation
prec_stats_mean[, quant_cv := ordered(quantcut(ens_cv, 5), labels = c('0-0.2', '0.2-0.4', '0.4-0.6', '0.6-0.8', '0.8-1.00'))]
prec_stats_mean_month[, quant_cv := ordered(quantcut(ens_cv, 5), labels = c('0-0.2', '0.2-0.4', '0.4-0.6', '0.6-0.8', '0.8-1.00')), month]

# Precipitation
prec_stats_mean[, prec_class := ordered(quantcut(ens_mean, 5), labels = c('low', 'below average', 'average', 'above average', 'high'))]

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
fname <- list.files(path = masks_dir_oro, full.names = TRUE, pattern = "mask_orography_groups_025.nc")
shape_mask <- raster(paste0(fname[1]))

shape_mask_crop <- crop(shape_mask, study_area)
shape_mask_df <- shape_mask_crop %>% as.data.frame(xy = TRUE, long = TRUE, na.rm = TRUE)
shape_mask_df <- subset(shape_mask_df, select = c('x', 'y', 'value'))
colnames(shape_mask_df) <- c('lon', 'lat', 'elev_class')

prec_stats_mean <- merge(prec_stats_mean, shape_mask_df, by = c('lon', 'lat'), all.x = T)
prec_stats_mean <- prec_stats_mean[complete.cases(prec_stats_mean)]

# Land use

fname <- list.files(path = masks_dir_landcover, full.names = TRUE, pattern = "mask_landcover_modis_025.nc")
shape_mask <- raster(paste0(fname[1]))

shape_mask_crop <- crop(shape_mask, study_area)
shape_mask_df <- shape_mask_crop %>% as.data.frame(xy = TRUE, long = TRUE, na.rm = TRUE)
shape_mask_df <- subset(shape_mask_df, select = c('x', 'y', 'value'))
colnames(shape_mask_df) <- c('lon', 'lat', 'land_class')

prec_stats_mean <- merge(prec_stats_mean, shape_mask_df, by = c('lon', 'lat'), all.x = T)
prec_stats_mean <- prec_stats_mean[complete.cases(prec_stats_mean)]

# Ecoregions (Biomes)

fname_shape <- list.files(path = masks_dir_ecoregions, full.names = TRUE, pattern = "mask_biomes_dinerstein.shp")
shape_mask <- st_read(paste0(fname_shape[1]))
shape_mask <- st_make_valid(shape_mask)

shape_mask_raster <- rasterize(shape_mask, prec_era5_kenya[[1]]) #directly rasterized; no cropping
#shape_raster_crop <- crop(shape_mask_raster, study_area)
shape_mask_df <- shape_mask_raster %>% as.data.frame(xy = TRUE, long = TRUE, na.rm = TRUE)
shape_mask_df <- subset(shape_mask_df, select = c('x', 'y', 'layer_BIOME_NUM'))
colnames(shape_mask_df) <- c('lon', 'lat', 'biome_class')

prec_stats_mean <- merge(prec_stats_mean, shape_mask_df, by = c('lon', 'lat'), all.x = T)
prec_stats_mean <- prec_stats_mean[complete.cases(prec_stats_mean)]

# Save for further use
prec_stats_mean_month <- merge(prec_stats_mean_month, prec_stats_mean[, .(lon, 
                                                                          lat, 
                                                                          prec_class, 
                                                                          KG_class, 
                                                                          elev_class, 
                                                                          land_class, 
                                                                          biome_class)], by = c("lon", "lat"))

saveRDS(prec_stats_mean, paste0(path_save_blueprint, "prec_mask_mean.rds"))
saveRDS(prec_stats_mean_month, paste0(path_save_blueprint, "prec_mask_mean_month.rds"))

## Quick validation
to_plot <- prec_stats_mean[quant_cv == '0-0.2']
p00 <- ggplot() +
  geom_raster(data = to_plot, aes(x = lon, y = lat, fill = "")) +
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




