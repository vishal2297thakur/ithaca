### Reading and subsetting data for the specified region and period
source('source/blueprint.R')
source('source/masks.R')
source('source/geo_functions.R')

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

clusterEvalQ(cluster, library(pRecipe))
parLapply(cluster, filenames, function(dummie){
  period_start <- lubridate::year(period_start)
  period_end <- lubridate::year(period_start)
  subset_time(dummie, c(period_start, period_end))
})

stopCluster(cluster)