# Reads and subsets data as netcdf files for the specified period.

source('source/main.R')
source('source/geo_functions.R')
source('source/era5land_budget.R')

library(pRecipe)

load(paste0(PATH_SAVE_ERA5LAND_BUDGET, "dataset_fpaths.Rdata"))

registerDoParallel(cores = N_CORES - 1)
n_prec_datasets <- length(prec_datasets_fpaths)
n_evap_datasets <- length(evap_datasets_fpaths)

prec_basins <- foreach(dataset_count = 1:n_prec_datasets, .combine = rbind) %dopar% {
  dummy <- brick(prec_datasets_fpaths[dataset_count])
  danube <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Danube_Europe.shp')))
  mexico <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Gulf of Mexico_North America.shp')))
  mahanadi <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Mahanadi_India.shp')))
  shebelli <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Shebelli_n_Juba_Africa.shp')))
  danube$basin <- 'danube'
  mexico$basin <- 'mexico'
  mahanadi$basin <- 'mahanadi'
  shebelli$basin <- 'shebelli'
  dummy_2 <- rbind(danube, mexico, mahanadi, shebelli)
  dummy_2$dataset <- prec_dataset_names[dataset_count]
  dummy_2
}

evap_basins <- foreach(dataset_count = 1:n_evap_datasets, .combine = rbind) %dopar% {
  dummy <- brick(evap_datasets_fpaths[dataset_count])
  danube <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Danube_Europe.shp')))
  mexico <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Gulf of Mexico_North America.shp')))
  mahanadi <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Mahanadi_India.shp')))
  shebelli <- fldmean(crop_data(dummy, paste0(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, '/era5_basins/Shebelli_n_Juba_Africa.shp')))
  danube$basin <- 'danube'
  mexico$basin <- 'mexico'
  mahanadi$basin <- 'mahanadi'
  shebelli$basin <- 'shebelli'
  dummy_2 <- rbind(danube, mexico, mahanadi, shebelli)
  dummy_2$dataset <- evap_dataset_names[dataset_count]
}

evap_basins <- rbind(evap_basins, dummy_2)

saveRDS(prec_basins, paste0(PATH_SAVE_ERA5LAND_BUDGET, 'prec_basins.rds'))
saveRDS(evap_basins, paste0(PATH_SAVE_ERA5LAND_BUDGET, 'evap_basins.rds'))

ggplot(prec_basins[year(date) > 1959]) +
  geom_line(aes(date, value, col = dataset)) +
  facet_wrap(~basin, scales = 'free') +
  theme_bw()


