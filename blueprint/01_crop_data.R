### Reading and subsetting data for the specified region and period

source('source/blueprint.R')
source('source/masks.R')
source('source/geo_functions.R')

datasets_kenya_2000_2019 <- list()

datasets_kenya_2000_2019 <- foreach(dataset_count = 1:n_datasets_2000_2019) %dopar% {
  result <- crop_space_time(brick(datasets_fnames_2000_2019[[dataset_count]]), period_start, period_end, study_area)
  writeRaster(result, 
              filename = paste0(path_save_blueprint, datasets_fnames_short_2000_2019[[dataset_count]], "_tp_mm_kenya_200101_201912_025_monthly.nc"),
              format= 'CDF')
  result
} 
names(datasets_kenya_2000_2019) <- datasets_fnames_short_2000_2019

datasets_kenya_1980_2019 <- foreach(dataset_count = 1:n_datasets_1980_2019) %dopar% {
  result <- crop_space_time(brick(datasets_fnames_2000_2019[[dataset_count]]), period_start, period_end, study_area)
  writeRaster(result, 
              filename = paste0(path_save_blueprint, datasets_fnames_short_1980_2019[[dataset_count]], "_tp_mm_kenya_200101_201912_025_monthly.nc"),
              format= 'CDF')
  result
} 
names(datasets_kenya_1980_2019) <- datasets_fnames_short_1980_2019

datasets_kenya_1960_2019 <- foreach(dataset_count = 1:n_datasets_1960_2019) %dopar% {
  result <- crop_space_time(brick(datasets_fnames_1960_2019[[dataset_count]]), period_start, period_end, study_area)
  writeRaster(result, 
              filename = paste0(path_save_blueprint, datasets_fnames_short_1960_2019[[dataset_count]], "_tp_mm_kenya_200101_201912_025_monthly.nc"),
              format= 'CDF')
  result
} 
names(datasets_kenya_1960_2019) <- datasets_fnames_short_1960_2019

## Save data for further use
saveRDS(datasets_kenya_2000_2019,  paste0(path_save_blueprint, "rasters_prec_kenya_2000_2019.rds"))
saveRDS(datasets_kenya_1980_2019,  paste0(path_save_blueprint, "rasters_prec_kenya_1980_2019.rds"))
saveRDS(datasets_kenya_1960_2019,  paste0(path_save_blueprint, "rasters_prec_kenya_1960_2019.rds"))

