# Review etmonitor data ----
source('source/partition_evap.R')
source("source/cdo_functions.R")
source("source/geo_functions.R")

library(pRecipe)

## geospatial 
library(raster)
library(ncdf4)
library(sp)
library(sf)
library(stars)

## Load data ----
fnames_nc <- list.files(path = PATH_EVAP_SIM, pattern = ".nc$", 
                        full.names = T)

fnames_product <- fnames_nc[grep("jra55", fnames_nc)]

fname <- fnames_product[grep("yearly", fnames_product)]

fname_tmp <- paste0(PATH_DATA, "sim/evap/raw//temp1.nc")
fname_tmp2 <- paste0(PATH_DATA, "sim/evap/raw//temp2.nc")

cmd_cdo <- paste0("cdo setmon,1 ", fname," ", fname_tmp )
system(cmd_cdo)
cmd_cdo <- paste0("cdo showdate ", fname_tmp)
system(cmd_cdo)
cmd_cdo <- paste0("cdo setday,1 ", fname_tmp, " ", fname_tmp2) 
system(cmd_cdo)
cmd_cdo <- paste0("cdo settime,00:00:00 ", fname_tmp2, " ", fname)
system(cmd_cdo)
cmd_cdo <- paste0("cdo showdate ", fname)
system(cmd_cdo)

### Remove temporary files ----
system(paste0("rm ",fname_tmp))
system(paste0("rm ",fname_tmp2))
