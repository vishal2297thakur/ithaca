### Additional packages and paths
source("source/main.R")

## paths
path_prec <- paste0(path_data,"sim/precip/raw/")
path_evap <- paste0(path_data,"sim/evap/raw/")
path_save_kenya <- paste0(path_save,"example_kenya/")

## variables
varname1 <- "tp" # shortname of the variable
varname2 <- "e" # shortname of the variable


# Specify the lat/lon for the region of analysis
lat_max <- 5 
lat_min <- -5.25
lon_min <- 33.75
lon_max <- 44.25

crop_box <- extent(lon_min, lon_max, lat_min, lat_max)

# Specify start/end for the period of analysis 
period_start <- as.Date("1960-01-01")
period_end <- as.Date("2019-12-01")



