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
  mexico <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Gulf of Mexico_North America.shp')))
  mahanadi <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Mahanadi_India.shp')))
  danube <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Danube_Europe.shp')))
  shebelli <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Shebelli_n_Juba_Africa.shp')))
  mexico$basin <- 'mexico'
  mahanadi$basin <- 'mahanadi'
  danube$basin <- 'danube'
  shebelli$basin <- 'shebelli'
  dummy_2 <- rbind(mexico, mahanadi, danube, shebelli)
  dummy_2$dataset <- prec_dataset_names[dataset_count]
  dummy_2
}

evap_basins <- foreach(dataset_count = 1:n_evap_datasets, .combine = rbind) %dopar% {
  dummy <- brick(evap_datasets_fpaths[dataset_count])
  mexico <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Gulf of Mexico_North America.shp')))
  mahanadi <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Mahanadi_India.shp')))
  danube <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Danube_Europe.shp')))
  shebelli <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Shebelli_n_Juba_Africa.shp')))
  mexico$basin <- 'mexico'
  mahanadi$basin <- 'mahanadi'
  danube$basin <- 'danube'
  shebelli$basin <- 'shebelli'
  dummy_2 <- rbind(mexico, mahanadi, danube, shebelli)
  dummy_2$dataset <- evap_dataset_names[dataset_count]
  dummy_2
}

runoff_basins <- foreach(dataset_count = 1:n_runoff_datasets, .combine = rbind) %dopar% {
  dummy <- brick(runoff_datasets_fpaths[dataset_count])
  mexico <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Gulf of Mexico_North America.shp')))
  mahanadi <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Mahanadi_India.shp')))
  danube <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Danube_Europe.shp')))
  shebelli <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Shebelli_n_Juba_Africa.shp')))
  mexico$basin <- 'mexico'
  mahanadi$basin <- 'mahanadi'
  danube$basin <- 'danube'
  shebelli$basin <- 'shebelli'
  dummy_2 <- rbind(mexico, mahanadi, danube, shebelli)
  dummy_2$dataset <- runoff_dataset_names[dataset_count]
  dummy_2
}

prec_basins$variable <- 'prec'
evap_basins$variable <- 'evap'
runoff_basins$variable <- 'runoff'
basins_eval <- rbind(prec_basins, evap_basins, runoff_basins)

saveRDS(basins_eval, paste0(PATH_SAVE_ERA5LAND_BUDGET, 'basins_eval.rds'))

## Evaluate
ggplot(basins_eval[year(date) > 1959 & variable == 'evap']) +
  geom_line(aes(date, value, col = dataset)) +
  facet_wrap(~basin, scales = 'free') +
  theme_bw()

dummy <- brick(prec_datasets_fpaths[1])
mexico_map <- crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Gulf of Mexico_North America.shp'))
mahanadi_map <- crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/mahanadi_India.shp'))
danube_map <- crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Danube_Europe.shp'))
shebelli_map <- crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Shebelli_n_Juba_Africa.shp'))

plot_map(mexico_map)
plot_map(mahanadi_map)
plot_map(danube_map)
plot_map(shebelli_map)
