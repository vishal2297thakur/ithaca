### Reading and subsetting data for the specified region and period

source('source/blueprint.R')
source('source/masks.R')
source('source/geo_functions.R')

fname_shape <- list.files(path = masks_dir_landsea, full.names = TRUE, pattern = "land_ocean")
shape_mask <- st_read(paste0(fname_shape[1]))
shape_mask <- st_make_valid(shape_mask)
shape_mask_crop <- st_crop(shape_mask, study_area)

datasets_fnames <- c(list.files(path = path_prec_sim, full.names = TRUE)[c(3, 9)],
  list.files(path = path_prec_obs, full.names = TRUE)[-c(7, 12, 14, 15)])
datasets_fnames_short <- c(list.files(path = path_prec_sim)[c(3, 9)],
                     list.files(path = path_prec_obs)[-c(7, 12, 14, 15)])
datasets_fnames_short <- strsplit(datasets_fnames_short, split='_', fixed = TRUE)
datasets_fnames_short <- sapply(datasets_fnames_short, "[[", 1)

n_datasets <- length(datasets_fnames)
datasets_kenya <- list()

datasets_kenya <- foreach(dataset_count = 1:n_datasets) %dopar% {
  result <- crop_space_time(brick(datasets_fnames[[dataset_count]]), period_start, period_end, study_area)
  #result <- st_crop(result, shape_mask)   ## TODO: Crop global data over land 
  writeRaster(dummy, paste0(path_save_blueprint, datasets_fnames_short[[dataset_count]], "_tp_mm_kenya_200101_201912_025_monthly.nc"),
              overwrite = TRUE)
  result
} 
names(datasets_kenya) <- datasets_fnames_short

shape_mask_crop <- st_crop(shape_mask, study_area)
shape_mask_raster <- rasterize(shape_mask_crop, prec_era5_kenya[[1]])
shape_mask_df <- shape_mask_raster %>% as.data.frame(xy = TRUE, long = TRUE, na.rm = TRUE)
shape_mask_df <- subset(shape_mask_df, select = c('x', 'y', 'value'))
colnames(shape_mask_df) <- c('lon', 'lat', 'land_ocean')
prec_stats_mean <- merge(prec_stats_mean, shape_mask_df, by = c('lon', 'lat'), all.x = T)
prec_stats_mean <- prec_stats_mean[land_ocean == "land"]
prec_stats_mean <- prec_stats_mean[, land_ocean := NULL]

## Save data for further use
saveRDS(datasets_kenya,  paste0(path_save_blueprint, "rasters_prec_kenya.rds"))



