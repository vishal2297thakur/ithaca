# Trend (Theil-Sen Slope) for each grid for global ensemble of all data products ----
source('source/evap_trend.R')
source('source/geo_functions.R')

library("Kendall")
library("RobustLinearReg")

## Data ----
evap_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets.rds"))

## Analysis ----
evap_datasets[, year := as.numeric(as.character(year))]
evap_datasets_grid_mean <- evap_datasets[, .(evap_mean = mean(evap, na.rm = T)), .(lat,lon,year)]

evap_trend_grid_ensemble <- evap_trend_lon_lat(evap_datasets_grid_mean)

evap_trend_grid_ensemble[lm_p_value > 0.05,significant_lm:= FALSE] 
evap_trend_grid_ensemble[lm_p_value <= 0.05,significant_lm:= TRUE] 

evap_trend_grid_ensemble[kendall_p_value > 0.05,significant_kendall:= FALSE] 
evap_trend_grid_ensemble[kendall_p_value <= 0.05,significant_kendall:= TRUE] 

evap_trend_grid_ensemble[theil_sen_p_value > 0.05,significant_theil_sen:= FALSE] 
evap_trend_grid_ensemble[theil_sen_p_value <= 0.05,significant_theil_sen:= TRUE] 

evap_trend_grid_ensemble[siegel_p_value > 0.05,significant_siegel:= FALSE] 
evap_trend_grid_ensemble[siegel_p_value <= 0.05,significant_siegel:= TRUE] 

## Save ----
saveRDS(evap_trend_grid_ensemble, paste0(PATH_SAVE_EVAP_TREND, "evap_trend_ensemble_grid.rds"))  
