### Assessing % change in ensemble mean between two 30 years time periods (t1=1960 to 1989 & t2=1990 to 2019) for each KG class

source('source/main.R')
source('source/graphics.R')
source('source/example_kenya.R')
source('source/masks.R')

# Read data 
prec_era5_kenya <- readRDS(paste0(path_save_kenya, "prec_era5.rds"))
prec_mean_change <- readRDS(paste0(path_save_kenya, "prec_mean_change.rds"))


fname_shape <- list.files(path = masks_dir_KG_beck, full.names = T, pattern = "climate_beck_level1.shp")
shape_mask <- st_read(paste0(fname_shape[1]))
shape_mask <- st_make_valid(shape_mask)

# Crop mask
shape_mask_crop <- st_crop(shape_mask, study_area)
shape_mask_raster <- rasterize(shape_mask_crop, prec_era5_kenya[[1]])
shape_mask_df <- shape_mask_raster %>% as.data.frame(xy = TRUE, long = TRUE, na.rm = TRUE)
shape_mask_df <- subset(shape_mask_df, select = c('x', 'y', 'value'))
colnames(shape_mask_df) <- c('lon', 'lat', 'KG_class')

# Main estimations
prec_mean_change <- merge(prec_mean_change, shape_mask_df, by = c('lon', 'lat'), all.x = T)

prec_mean_change[, perc_change_KG := round(mean(perc_change), 2), KG_class]
prec_mean_change[, diff_KG := round(mean(diff), 1), KG_class]

prec_mean_change[, unique(perc_change_KG), KG_class]
prec_mean_change[, unique(diff_KG), KG_class]
