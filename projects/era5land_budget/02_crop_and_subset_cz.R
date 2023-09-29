# Reads and subsets data as netcdf files for the specified period.

source('source/main.R')
source('source/era5land_budget.R')

library(pRecipe)

load(paste0(PATH_SAVE_ERA5LAND_BUDGET, "dataset_fpaths.Rdata"))

registerDoParallel(cores = N_CORES - 1)
n_prec_datasets <- length(prec_datasets_fpaths)
n_evap_datasets <- length(evap_datasets_fpaths)
n_runoff_datasets <- length(runoff_datasets_fpaths)

prec_basins <- foreach(dataset_count = 1:n_prec_datasets, .combine = rbind) %dopar% {
  dummy <- brick(prec_datasets_fpaths[dataset_count])
  czechia <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/CZE_adm0.shp')))
  czechia$basin <- 'czechia'
  czechia$dataset <- prec_dataset_names[dataset_count]
  czechia
}

evap_basins <- foreach(dataset_count = 1:n_evap_datasets, .combine = rbind) %dopar% {
  dummy <- brick(evap_datasets_fpaths[dataset_count])
  czechia <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/CZE_adm0.shp')))
  czechia$basin <- 'czechia'
  czechia$dataset <- evap_dataset_names[dataset_count]
  czechia
}

runoff_basins <- foreach(dataset_count = 1:n_runoff_datasets, .combine = rbind) %dopar% {
  dummy <- brick(runoff_datasets_fpaths[dataset_count])
  czechia <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/CZE_adm0.shp')))
  czechia$basin <- 'czechia'
  czechia$dataset <- runoff_dataset_names[dataset_count]
  czechia
}

prec_basins$variable <- 'prec'
evap_basins$variable <- 'evap'
runoff_basins$variable <- 'runoff'
basins_eval <- rbind(prec_basins, evap_basins, runoff_basins)

prec_basins_era5land_raster <- brick('~/shared/data/sim/precip/raw/era5-land_tp_mm_land_195001_202112_025_monthly.nc')
prec_basins_era5land <- fldmean(crop_data(prec_basins_era5land_raster, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/CZE_adm0.shp')))
prec_basins_era5land$basin <- 'czechia'
prec_basins_era5land$dataset <- "era5land"
prec_basins_era5land$variable <- "prec"
basins_eval <- rbind(basins_eval, prec_basins_era5land)

saveRDS(basins_eval, paste0(PATH_SAVE_ERA5LAND_BUDGET, 'basins_eval_czechia.rds'))

## Evaluate
ggplot(basins_eval[year(date) > 1959 & variable == 'evap']) +
  geom_line(aes(date, value, col = dataset)) +
  facet_wrap(~basin, scales = 'free') +
  theme_bw()

dummy <- brick(evap_datasets_fpaths[1])
czechia_map <- crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/CZE_adm0.shp'))

plot_map(czechia_map)
