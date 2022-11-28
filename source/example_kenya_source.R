### Additional packages and paths
source("source/main.R")

## packages
# gis
library(ncdf4)
library(sp)
library(rgdal) 
library(sf)

# plotting
library(ggplot2)


## paths
paths_var1 <- paste0(path_data,"sim/precip/raw")
paths_var2 <- paste0(path_data,"sim/evap/raw")

## variables
varname1 <- "tp" # shortname of the variable
varname2 <- "e" # shortname of the variable


# Specify the lat/lon for the region of analysis
lat_top <- 5 
lat_bottom <- -5.25
lon_left <- 33.75
lon_right <- 44.25

crop_box <- extent(lon_left, lon_right, lat_bottom, lat_top)
