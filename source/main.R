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
path_prec_sim <- paste0(path_data, "sim/precip/raw/")
path_prec_obs <- paste0(path_data, "obs/precip/raw/")
path_evap_sim <- paste0(path_data, "sim/evap/raw/")
path_evap_obs <- paste0(path_data, "obs/evap/raw/")

##Datasets
#Datasets with common periods
load('~/shared/data_projects/ithaca/misc/common_periods.Rdata') #Created in database/scripts/05_shared period_dataset.R

#Types
prec_datasets_obs <- c("cpc", "cru-ts", "em-earth", "ghcn", "gpcc", "precl", "udel")
prec_datasets_reanal <- c("20cr", "era20c", "era5", "ncep-doe", "ncep-ncar")
prec_datasets_remote <- c("chirps", "cmap", "cmorph", "gpcp", "gpm-imerg", "mswep", "persiann", "trmm-3b43")
prec_datasets_hydrol <- c("gldas-clsm", "gldas-noah", "gldas-vic", "terraclimate")

##Constants
#Time
PERIOD_START <- as.Date("1960-01-01")
PERIOD_END <- as.Date("2019-12-31")

PERIOD_1_START <- as.Date("1960-01-01")
PERIOD_1_END <- as.Date("1989-12-31")
PERIOD_2_START <- as.Date("1990-01-01")
PERIOD_2_END <- as.Date("2019-12-31")

#Space
PILOT_LAT_MAX <- 5 
PILOT_LAT_MIN <- -5.25
PILOT_LON_MIN  <- 33.75
PILOT_LON_MAX <- 44.25

##Variable names
prec_name <- "prec"
evap_name <- "evap"

prec_name_short <- "TP"
evap_name_short <- "E"

##Parallelization
cores_n <- detectCores()
registerDoParallel(cores = cores_n - 2)

##Functions
