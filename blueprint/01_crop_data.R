### Reading and subsetting data for the specified region and period

source('source/blueprint.R')
source('source/masks.R')
source('source/geo_functions.R')

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
  writeRaster(result, 
              filename = paste0(path_save_blueprint, datasets_fnames_short[[dataset_count]], "_tp_mm_kenya_200101_201912_025_monthly.nc"),
              format= 'CDF', overwrite = TRUE)
  result
} 
names(datasets_kenya) <- datasets_fnames_short

## Save data for further use
saveRDS(datasets_kenya,  paste0(path_save_blueprint, "rasters_prec_kenya.rds"))


