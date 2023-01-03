## packages
# generic
library(data.table)
library(lubridate)

# plotting
library(ggplot2)

# geospatial
library(raster)
library(ncdf4)
library(sp)
library(sf)

# parallel
library(doParallel)

##Paths
path_data <- '~/shared/data/'
path_data_review <- '~/shared/data_review/'
path_save <- '~/shared/data_projects/ithaca/'

##Constants
PERIOD_START <- as.Date("1960-01-01")
PERIOD_END <- as.Date("2019-12-01")

PILOT_LAT_MAX <- 5 
PILOT_LAT_MIN <- -5.25
PILOT_LON_MIN  <- 33.75
PILOT_LON_MAX <- 44.25

##Variable names
prec_name <- "prec"
evap_name <- "evap"

prec_name_short <- "tp"
evap_name_short <- "e"

##Parallelization
cores_n <- detectCores()
registerDoParallel(cores = cores_n - 2)

##Functions
