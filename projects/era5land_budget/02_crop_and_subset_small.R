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
  austin <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Austin_North America.shp')))
  dhalegaon <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Dhalegaon_India.shp')))
  nagymaros <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Nagymaros_Europe.shp')))
  lugh_ganana <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Lugh Ganana_Africa.shp')))
  austin$basin <- 'austin'
  dhalegaon$basin <- 'dhalegaon'
  nagymaros$basin <- 'nagymaros'
  lugh_ganana$basin <- 'lugh_ganana'
  dummy_2 <- rbind(austin, dhalegaon, nagymaros, lugh_ganana)
  dummy_2$dataset <- prec_dataset_names[dataset_count]
  dummy_2
}

evap_basins <- foreach(dataset_count = 1:n_evap_datasets, .combine = rbind) %dopar% {
  dummy <- brick(evap_datasets_fpaths[dataset_count])
  austin <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Austin_North America.shp')))
  dhalegaon <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Dhalegaon_India.shp')))
  nagymaros <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Nagymaros_Europe.shp')))
  lugh_ganana <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Lugh Ganana_Africa.shp')))
  austin$basin <- 'austin'
  dhalegaon$basin <- 'dhalegaon'
  nagymaros$basin <- 'nagymaros'
  lugh_ganana$basin <- 'lugh_ganana'
  dummy_2 <- rbind(austin, dhalegaon, nagymaros, lugh_ganana)
  dummy_2$dataset <- evap_dataset_names[dataset_count]
  dummy_2
}

runoff_basins <- foreach(dataset_count = 1:n_runoff_datasets, .combine = rbind) %dopar% {
  dummy <- brick(runoff_datasets_fpaths[dataset_count])
  austin <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Austin_North America.shp')))
  dhalegaon <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Dhalegaon_India.shp')))
  nagymaros <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Nagymaros_Europe.shp')))
  lugh_ganana <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Lugh Ganana_Africa.shp')))
  austin$basin <- 'austin'
  dhalegaon$basin <- 'dhalegaon'
  nagymaros$basin <- 'nagymaros'
  lugh_ganana$basin <- 'lugh_ganana'
  dummy_2 <- rbind(austin, dhalegaon, nagymaros, lugh_ganana)
  dummy_2$dataset <- runoff_dataset_names[dataset_count]
  dummy_2
}

prec_basins$variable <- 'prec'
evap_basins$variable <- 'evap'
runoff_basins$variable <- 'runoff'
basins_eval <- rbind(prec_basins, evap_basins, runoff_basins)

saveRDS(basins_eval, paste0(PATH_SAVE_ERA5LAND_BUDGET, 'basins_eval_small.rds'))

## Evaluate
ggplot(basins_eval[year(date) > 1959 & variable == 'evap']) +
  geom_line(aes(date, value, col = dataset)) +
  facet_wrap(~basin, scales = 'free') +
  theme_bw()

dummy <- brick(evap_datasets_fpaths[1])
austin_map <- crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Austin_North America.shp'))
dhalegaon_map <- crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Dhalegaon_India.shp'))
nagymaros_map <- crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Nagymaros_Europe.shp'))
lugh_ganana_map <- crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Lugh Ganana_Africa.shp'))

plot_map(austin_map)
plot_map(dhalegaon_map)
plot_map(nagymaros_map)
plot_map(lugh_ganana_map)
