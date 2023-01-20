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
#06/2000-12/2019
prec_fnames_2000_2019 <- c(list.files(path = path_prec_sim, full.names = TRUE),
                     list.files(path = path_prec_obs, full.names = TRUE))
grep("land", prec_fnames_2000_2019, value = TRUE)[c(2, 8:10, 12:19, 12:16)]
prec_fnames_2000_2019 <- 
prec_fnames_short <- c(list.files(path = path_prec_sim)[c(3, 9)],
                           list.files(path = path_prec_obs)[-c(7, 12, 14, 15)])
prec_fnames_short <- strsplit(prec_fnames_short, split = '_', fixed = TRUE)
prec_fnames_short_2000_2019 <- sapply(prec_fnames_short, "[[", 1)

#01/1981-12/2019

#01/1961-12/2019

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
