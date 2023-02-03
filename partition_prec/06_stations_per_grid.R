# Plot number of stations per grid cell

source('source/partition_prec.R')
source('source/geo_functions.R')
source('source/graphics.R')

# Data
prec_grid <- raster(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_grid.nc")) %>% brick_to_dt()
