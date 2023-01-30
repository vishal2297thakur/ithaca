### Reading and subsetting data for the specified region and period

source('source/blueprint.R')
source('source/masks.R')
source('source/geo_functions.R')

foreach(dataset_count = 1:n_datasets_2000_2019) %dopar% {
  result <- crop_time(brick(prec_fnames_2000_2019[[dataset_count]]),
                            period_start, period_end)
  nc_out <- paste0(path_save_blueprint,
                   prec_fnames_short_2000_2019[[dataset_count]],
                   "_tp_mm_land_200001_201912_025_monthly.nc")
  save_nc(result, nc_out)
} 

filenames <- prec_fnames_2000_2019
no_cores <- detectCores()
no_cores <- no_cores - 1
cluster <- makeCluster(no_cores, type = "PSOCK")

clusterExport(cluster, c('period_start', 'period_end', 'crop_time'), envir = environment())
clusterEvalQ(cluster, library(raster))
clusterEvalQ(cluster, library(data.table))
clusterEvalQ(cluster, library(lubridate))
dummie_list <- parLapply(cluster, filenames, function(dummie){
  dummie <-  brick(dummie)
  crop_time(dummie, period_start, period_end)
})
stopCluster(cluster)

foreach(dataset_count = 1:n_datasets_1980_2019) %dopar% {
  result <- crop_space_time(brick(prec_fnames_1980_2019[[dataset_count]]),
                            period_start, period_end, study_area)
  nc_out <- paste0(path_save_blueprint,
                   prec_fnames_short_1980_2019[[dataset_count]],
                   "_tp_mm_kenya_198001_201912_025_monthly.nc")
  save_nc(result, nc_out)
} 

foreach(dataset_count = 1:n_datasets_1960_2019) %dopar% {
  result <- crop_space_time(brick(prec_fnames_1960_2019[[dataset_count]]),
                            period_start, period_end, study_area)
  nc_out <- paste0(path_save_blueprint, prec_fnames_short_1960_2019[[dataset_count]],
                   "_tp_mm_kenya_196001_201912_025_monthly.nc")
  save_nc(result, nc_out)
} 

