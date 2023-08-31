# Reads and subsets data as netcdf files for the specified period.

source('source/main.R')
source('source/geo_functions.R')
source('source/era5land_budget.R')

library(pRecipe)
load(paste0(PATH_SAVE_ERA5LAND_BUDGET, "dataset_fpaths.Rdata"))

prec_datasets_fpaths
test <- raster::brick(prec_datasets_fpaths[1])
test_bas_1 <- crop_data(test, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Danube_Europe.shp'))
test_bas_1_ts <- fldmean(test_bas_1)

