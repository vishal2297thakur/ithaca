### Add categorical classes to each variable 
install.packages("gtools")
library(gtools)
library(plyr)

source('source/blueprint.R')
source('source/geo_functions.R')
source('source/graphics.R')
source('source/masks.R')

## Read data 
prec_era5_kenya <- brick(paste0(path_save_blueprint, "era5_tp_mm_kenya_200006_201912_025_monthly.nc"))
prec_stats <- readRDS(paste0(path_save_blueprint, "prec_ensemble_stats.rds"))
prec_stats_month <- readRDS(paste0(path_save_blueprint, "prec_ensemble_stats_month.rds"))

## Masks
# Bias: Coefficient of Variation
prec_stats[, quant_cv := ordered(quantcut(ens_mean_cv, 5), labels = c('0-0.2', '0.2-0.4', '0.4-0.6', '0.6-0.8', '0.8-1.00'))]
prec_stats_month[, quant_cv := ordered(quantcut(ens_mean_cv, 5), labels = c('0-0.2', '0.2-0.4', '0.4-0.6', '0.6-0.8', '0.8-1.00')), month]

## Masks
# Precipitation
prec_stats[, prec_class := ordered(quantcut(ens_mean_mean, 5), labels = c('low', 'below average', 'average', 'above average', 'high'))]

# Koppen-Geiger
fname_shape <- list.files(path = masks_dir_KG_beck, full.names = TRUE, pattern = "climate_beck_level3.shp")
shape_mask <- st_read(paste0(fname_shape[1]))
shape_mask <- st_make_valid(shape_mask)

shape_mask_crop <- st_crop(shape_mask, study_area)
shape_mask_raster <- rasterize(shape_mask_crop, prec_era5_kenya[[1]])
shape_mask_df <- shape_mask_raster %>% as.data.frame(xy = TRUE, long = TRUE, na.rm = TRUE)
shape_mask_df <- subset(shape_mask_df, select = c('x', 'y', 'value'))
colnames(shape_mask_df) <- c('lon', 'lat', 'KG_class')
shape_mask_df$KG_class <- factor(shape_mask_df$KG_class)

prec_stats <- merge(prec_stats, shape_mask_df, by = c('lon', 'lat'))
prec_stats <- prec_stats[complete.cases(prec_stats)]

# Elevation
fname <- list.files(path = masks_dir_oro, full.names = TRUE, pattern = "mask_orography_groups_025.nc")
shape_mask <- raster(paste0(fname[1]))
shape_mask <- ratify(shape_mask)

mask_fname <- list.files(path = masks_dir_oro, pattern = "*groups_025_classes.txt" , full.names = T)
mask_raster_classes <- read.table(paste(mask_fname[1]))
mask_raster_classes <- as.data.frame(sapply(mask_raster_classes,
                         mapvalues, from = c("(-Inf,100]", "(800,1.5e+03]", "(1.5e+03,3e+03]", "(3e+03, Inf]"), 
                         to = c("(0,100]", "(800,1500]", "(1500,3000]", "(3000,Inf]")))
levels(shape_mask)[[1]] <- mask_raster_classes

shape_mask_crop <- crop(shape_mask, study_area)
shape_mask_df <- shape_mask_crop %>% as.data.frame(xy = TRUE, long = TRUE, na.rm = TRUE)
shape_mask_df <- subset(shape_mask_df, select = c('x', 'y', 'value'))
colnames(shape_mask_df) <- c('lon', 'lat', 'elev_class')
shape_mask_df$elev_class <- factor(shape_mask_df$elev_class, 
                                   levels = c("(0,100]", "(100,400]", "(400,800]", "(800,1500]", "(1500,3000]", "(3000,Inf]"), 
                                   labels = c("0-100", "100-400", "400-800", "800-1500", "1500-3000", "3000+"), 
                                   ordered =TRUE)

prec_stats <- merge(prec_stats, shape_mask_df, by = c('lon', 'lat'))
prec_stats <- prec_stats[complete.cases(prec_stats)]

# Land use

fname <- list.files(path = masks_dir_landcover, full.names = TRUE, pattern = "mask_landcover_modis_025.nc")
shape_mask <- raster(paste0(fname[1]))
shape_mask <- ratify(shape_mask)

mask_fname <- list.files(path = masks_dir_landcover, pattern = "*modis_025_classes.txt" , full.names = T)
mask_raster_classes <- read.table(paste(mask_fname[1]))
levels(shape_mask) <- mask_raster_classes

shape_mask_crop <- crop(shape_mask, study_area)
shape_mask_df <- shape_mask_crop %>% as.data.frame(xy = TRUE, long = TRUE, na.rm = TRUE)
shape_mask_df <- subset(shape_mask_df, select = c('x', 'y', 'value'))
colnames(shape_mask_df) <- c('lon', 'lat', 'land_class')
shape_mask_df$land_class <- factor(shape_mask_df$land_class)

prec_stats <- merge(prec_stats, shape_mask_df, by = c('lon', 'lat'))
prec_stats <- prec_stats[complete.cases(prec_stats)]

# Save for further use
prec_masks <- prec_stats[, .(lon, lat, prec_mean = ens_mean_mean, quant_ens_cv, rel_dataset_agreement, 
                             abs_dataset_agreement, outlier_dataset, prec_class, 
                             KG_class, elev_class, land_class)]
prec_masks_month <- merge(prec_stats_month, prec_masks[, .(lon, 
                                                           lat, 
                                                           prec_class, 
                                                           KG_class, 
                                                           elev_class, 
                                                           land_class)], 
                          by = c("lon", "lat"), all.y = TRUE)

saveRDS(prec_masks, paste0(path_save_blueprint, "prec_masks.rds"))
saveRDS(prec_masks_month, paste0(path_save_blueprint, "prec_masks_month.rds"))

## Quick validation
to_plot <- prec_stats
p00 <- ggplot() +
  geom_raster(data = to_plot, aes(x = lon, y = lat, fill = land_class)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(to_plot$lon), max(to_plot$lon)), 
                  ylim = c(min(to_plot$lat), max(to_plot$lat))) +  
  labs(x = "", y = "", fill = 'Class') +
  scale_x_continuous(expand = c(0.015, 0.015)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.grid = element_line(color = "grey")) 

y_labs <- ggplot_build(p00)$layout$panel_params[[1]]$y$get_labels()
x_labs <- ggplot_build(p00)$layout$panel_params[[1]]$x$get_labels()
p01 <- p00 + scale_x_continuous(expand = c(0.015, 0.015), labels = paste0(x_labs, "\u00b0")) +
  scale_y_continuous(expand = c(0.0125, 0.0125),  labels = paste0(y_labs, "\u00b0"))
p01




