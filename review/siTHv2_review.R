# Review SiTHv2 evaporation data ----
source('source/partition_evap.R')
source("source/cdo_functions.R")

library(pRecipe)

## geospatial 
library(raster)
library(ncdf4)
library(sp)
library(sf)
library(stars)

## Load data ----
fnames_nc <- list.files(path = "~/shared/data_review", pattern = ".nc$", 
                        full.names = T)

fnames_product <- fnames_nc[grep("SiTHv2", fnames_nc)]

fname <- fnames_product[grep("yearly", fnames_product)]

## 1. Check name: Data product, variables, unit, scale, beginning of time, end of time, spatial resolution, temporal resolution ----
fname

## 2. Check unit conversion/scaling (nc history)----
# multipliers "-mulc"
# muldpm
cmd_ncdump <- paste0("ncdump -h ", fname)
system(cmd_ncdump)

# variable name
cmd_cdo_variable_name <- paste0("cdo vardes ", fname)
system(cmd_cdo_variable_name)

# summary
cdo_info_fnc(fname)

## 3. plot map to see if it looks okay ----
data <- brick(fname) 
pRecipe::plot_map(data[[12]])

test <- pRecipe::tabular(data)
test2 <- brick_to_dt(data)


## 4. Check projection ----
# projection = lonlat
# lon: from east to west and -180 to 180 (1440)
# lat:from north to south and 90 to -90 (720)
crs(data)
extent(data)

# OG grid location for comparison
cmd_cdo_griddes <- paste0("cdo -griddes ", fname)
system(cmd_cdo_griddes)

grid_location <- "/mnt/shared/data_projects/ithaca/misc/grid_ithaca"
cmd_cat_grid <- paste0("cat ", grid_location)
system(cmd_cat_grid)

## 5. Check time units ----
# daily: hours should always be 00:00:00
# monthly: day should always be 01
# yearly: month and day should be 01-01
cdo_sinfo_fnc(fname)
data@z$Date

## 6. Check data type: Data type F32z ----
# scroll to top of output
cdo_sinfo_fnc(fname)

## 7. Compare values to at least one other publication ----
# Change according to need
# Comparison to Kim et all. 2021 "An Assessment of Concurrency in Evapotranspiration Trends across Multiple Global Datasets" reported mean value form 1982-2012 
period_start <- as.Date("1982-01-01") 
period_end <- as.Date("2012-01-01") 

start_time <- which(data@z$Date == period_start)
end_time <- which(data@z$Date == period_end)

# selects time
cdo_seltimestep_fnc(inputfile_name = fname, outputfile_name = "~/Review/siTHv2_review_sel.nc", start_time = start_time, end_time = end_time)

# Calculate field means
cdo_fldmean_fnc(inputfile_name = "~/Review/siTHv2_review_sel.nc", outputfile_name = "~/Review/siTHv2_review_fldmean_check.nc")

cdo_info_fnc(inputfile_name = "~/Review/siTHv2_review_fldmean_check.nc")

cdo_timmean_fnc(inputfile_name =  "~/Review/siTHv2_review_fldmean_check.nc", outputfile_name = "~/Review/siTHv2_fldmean_timmean_mean.nc")

cdo_info_fnc(inputfile_name = "~/Review/siTHv2_fldmean_timmean_mean.nc")
