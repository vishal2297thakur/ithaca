# Review gleam data ----
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

## stats
library("Kendall")
library("RobustLinearReg")

## Load data ----
fnames_nc <- list.files(path = PATH_EVAP_SIM, pattern = ".nc$", 
                        full.names = T)

fnames_product <- fnames_nc[grep("gleam", fnames_nc)]

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
period_start <- as.Date("1982-01-01") 
period_end <- as.Date("2020-01-01") 

start_time <- which(data@z$Date == period_start)
end_time <- which(data@z$Date == period_end)

cdo_seltimestep_fnc(inputfile_name = fname, outputfile_name = "~/Review/gleam_review_sel_i.nc", start_time = start_time, end_time = end_time)
cdo_fldmean_fnc(inputfile_name = "~/Review/gleam_review_sel_i.nc", outputfile_name = "~/Review/gleam_review_sel_fldmean.nc")

cmd_cdo_trend <- paste0("cdo trend ~/Review/gleam_review_sel_fldmean.nc ~/Review/gleam_review_sel_fldmean_trend_a.nc ~/Review/gleam_review_sel_fldmean_trend_b.nc")
system(cmd_cdo_trend)
cdo_info_fnc(inputfile_name = "~/Review/gleam_review_sel_fldmean_trend_b.nc")

fld_means <- cdo_info_to_text_fnc(inputfile_name = "~/Review/gleam_review_sel_fldmean.nc", outputfile_name = "~/Review/gleam_1982_2020.txt")
fld_means[, year := as.numeric(format(date, "%Y"))]
slope <- theil_sen_regression(value ~ year, data = fld_means)$coefficients[2]
lm(value ~ year, data = fld_means)
mean_fld <- mean(fld_means$value, na.rm = T)
plot(fld_means$date, fld_means$value-mean_fld, type = "b", ylim = c(-30, 30))
