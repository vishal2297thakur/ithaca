# Reading and subsetting data for the specified region and period

source('source/med_arid.R')
source('source/geo_functions.R')

registerDoParallel(cores = N_CORES - 1)

foreach(dataset_count = 1:n_datasets_2000_2019) %dopar% {
  result <- crop_space_time(brick(PREC_FNAMES_2000_2019_FULL_RECORD[[dataset_count]]),
                            period_start, period_end, study_area)
  nc_out <- paste0(PATH_SAVE_MED_ARID_RAW,
                   PREC_FNAMES_SHORT_2000_2019_FULL_RECORD[[dataset_count]],
                   "_tp_mm_mediterranean_200001_201912_025_yearly.nc")
  save_nc(result, nc_out)
} 

foreach(dataset_count = 1:n_datasets_2000_2019) %dopar% {
  result <- crop_space_time(brick(EVAP_FNAMES_2000_2019_FULL_RECORD[[dataset_count]]),
                            period_start, period_end, study_area)
  nc_out <- paste0(PATH_SAVE_MED_ARID_RAW,
                   EVAP_FNAMES_SHORT_2000_2019_FULL_RECORD[[dataset_count]],
                   "_e_mm_mediterranean_200001_201912_025_yearly.nc")
  save_nc(result, nc_out)
} 

