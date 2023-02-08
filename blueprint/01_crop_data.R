# Reading and subsetting data for the specified region and period
source('source/blueprint.R')
source('source/geo_functions.R')

foreach(dataset_count = 1:n_datasets_2000_2019) %dopar% {
  result <- crop_space_time(brick(PREC_FNAMES_2000_2019_FULL_RECORD[[dataset_count]]),
                            period_start, period_end, study_area)
  nc_out <- paste0(PATH_SAVE_BLUEPRINT_RAW,
                   PREC_FNAMES_SHORT_2000_2019[[dataset_count]],
                   "_tp_mm_kenya_200001_201912_025_monthly.nc")
  save_nc(result, nc_out)
} 