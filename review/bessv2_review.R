# Review bess data ----
source('source/partition_evap.R')
source("source/cdo_functions.R")

library(pRecipe)

## geospatial 
library(raster)
library(ncdf4)
library(sp)
library(sf)
library(stars)

## stats
library("openair")

## Load data ----
fnames_nc <- list.files(path = PATH_EVAP_SIM, pattern = ".nc$", 
                        full.names = T)

fnames_product <- fnames_nc[grep("bess", fnames_nc)]

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
cdo_timmean_fnc(inputfile_name = fname, outputfile_name = "~/Review/bess_review_time_mean.nc")
cdo_info_fnc(inputfile_name = "~/Review/bess_review_time_mean.nc")

# Change according to need
# Comparison to product paper (Li et al. 2023, Remote Sensing) reporting annual volume average of ET of 67.7 10E3 from 1982-2019
period_start <- as.Date("1982-01-01") 
period_end <- as.Date("2019-01-01") 

start_time <- which(data@z$Date == period_start)
end_time <- which(data@z$Date == period_end)

# selects time
cdo_seltimestep_fnc(inputfile_name = fname, outputfile_name = "~/Review/bess_review_sel.nc", start_time = start_time, end_time = end_time)

cdo_timmean_fnc(inputfile_name =  "~/Review/bess_review_sel.nc", outputfile_name = "~/Review/bess_timmean_mean.nc")
system(paste0("cdo gridarea ~/Review/bess_review_sel.nc ~/Review/bess_review_grid.nc"))
cdo_info_fnc("~/Review/bess_review_grid.nc")

cdo_fldsum_fnc(inputfile_name = "~/Review/bess_review_grid.nc", outputfile_name = "~/Review/bess_review_grid_sum.nc")
cdo_info_fnc(inputfile_name = "~/Review/bess_review_grid_sum.nc")

system(paste0("cdo mul ~/Review/bess_timmean_mean.nc ~/Review/bess_review_grid.nc ~/Review/bess_timmean_mean_area.nc"))
cdo_sinfo_fnc("~/Review/bess_timmean_mean_area.nc")

cdo_fldsum_fnc(inputfile_name = "~/Review/bess_timmean_mean_area.nc", outputfile_name = "~/Review/bess_review_timmean_mean_area_fldsum.nc")
cdo_info_fnc(inputfile_name = "~/Review/bess_review_timmean_mean_area_fldsum.nc")
cdo_info_fnc(inputfile_name = "~/Review/bess_timmean_mean_area.nc")

6.7167e+16*M2_TO_KM2*MM_TO_KM
6.7167e+16*M2_TO_KM2*MM_TO_KM/GLOBAL_AREA

cdo_fldmean_fnc(inputfile_name = "~/Review/bess_review_sel.nc", outputfile_name = "~/Review/bess_review_sel_fldmean.nc")
cdo_info_fnc(inputfile_name = "~/Review/bess_review_sel_fldmean.nc")
fld_means <- cdo_info_to_text_fnc(inputfile_name = "~/Review/bess_review_sel_fldmean.nc", outputfile_name = "~/Review/bess_1982_2019.txt")
fld_means[, year := as.numeric(format(date, "%Y"))]
fld_means[, date := paste0(year, "-01-01 00:00:00")]
fld_means[, date := as.POSIXct(date)]
slope <- TheilSen(fld_means, pollutant = "value", autocor = FALSE, plot = F, silent = T)
lm(value ~ year, data = fld_means)

plot(fld_means$year, fld_means$value*M2_TO_KM2*MM_TO_KM*GLOBAL_AREA/1000, ylim = c(60, 110))
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray", # Grid line color
     lwd = 2)     
lm(value*M2_TO_KM2*MM_TO_KM*GLOBAL_AREA/1000 ~ year, data = fld_means)

cmd_cdo_trend <- paste0("cdo trend ~/Review/bess_review_sel_fldmean.nc ~/Review/bess_review_sel_fldmean_trend_a.nc ~/Review/bess_review_sel_fldmean_trend_b.nc")
system(cmd_cdo_trend)
cdo_info_fnc(inputfile_name = "~/Review/bess_review_sel_fldmean_trend_b.nc")
cdo_info_fnc(inputfile_name = "~/Review/bess_review_sel_fldmean_trend_a.nc")

-0.30*M2_TO_KM2*MM_TO_KM*GLOBAL_AREA

# Comparison to review paper (yang et al. 2023, Nature Reviews Earth & Environment) reporting trend for (i) 1982-2016 - 0.12 mm/yr/yr, (ii) 1982-2011 (SI) - 0.2 mm/yr/yr
# trend i
period_start <- as.Date("1982-01-01") 
period_end <- as.Date("2016-01-01") 

start_time <- which(data@z$Date == period_start)
end_time <- which(data@z$Date == period_end)

cdo_seltimestep_fnc(inputfile_name = fname, outputfile_name = "~/Review/bess_review_sel_i.nc", start_time = start_time, end_time = end_time)
cdo_fldmean_fnc(inputfile_name = "~/Review/bess_review_sel_i.nc", outputfile_name = "~/Review/bess_review_sel_fldmean.nc")
cmd_cdo_trend <- paste0("cdo trend ~/Review/bess_review_sel_fldmean.nc ~/Review/bess_review_sel_fldmean_trend_a.nc ~/Review/bess_review_sel_fldmean_trend_b.nc")
system(cmd_cdo_trend)
cdo_info_fnc(inputfile_name = "~/Review/bess_review_sel_fldmean_trend_b.nc")
cdo_info_fnc(inputfile_name = "~/Review/bess_review_sel_fldmean_trend_a.nc")

cdo_info_fnc("~/Review/bess_review_sel_fldmean.nc")
fld_means <- cdo_info_to_text_fnc(inputfile_name = "~/Review/bess_review_sel_fldmean.nc", outputfile_name = "~/Review/bess_1982_2016.txt")
fld_means[, year := as.numeric(format(date, "%Y"))]
fld_means[, year := as.numeric(format(date, "%Y"))]
fld_means[, date := paste0(year, "-01-01 00:00:00")]
fld_means[, date := as.POSIXct(date)]
slope <- TheilSen(fld_means, pollutant = "value", autocor = FALSE, plot = F, silent = T)
lm(value ~ year, data = fld_means)
mean_fld <- mean(fld_means$value, na.rm = T)
plot(fld_means$date, fld_means$value-mean_fld, type = "b", ylim = c(-30, 30))

# trend ii
period_start <- as.Date("1982-01-01") 
period_end <- as.Date("2011-01-01") 

start_time <- which(data@z$Date == period_start)
end_time <- which(data@z$Date == period_end)

cdo_seltimestep_fnc(inputfile_name = fname, outputfile_name = "~/Review/bess_review_sel_ii.nc", start_time = start_time, end_time = end_time)
cdo_fldmean_fnc(inputfile_name = "~/Review/bess_review_sel_ii.nc", outputfile_name = "~/Review/bess_review_sel_fldmean.nc")
cmd_cdo_trend <- paste0("cdo trend ~/Review/bess_review_sel_fldmean.nc ~/Review/bess_review_sel_fldmean_trend_a.nc ~/Review/bess_review_sel_fldmean_trend_b.nc")
system(cmd_cdo_trend)
cdo_info_fnc(inputfile_name = "~/Review/bess_review_sel_fldmean_trend_b.nc")
cdo_info_fnc("~/Review/bess_review_sel_fldmean.nc")
fld_means <- cdo_info_to_text_fnc(inputfile_name = "~/Review/bess_review_sel_fldmean.nc", outputfile_name = "~/Review/bess_1982_2011.txt")
fld_means[, year := as.numeric(format(date, "%Y"))]
fld_means[, year := as.numeric(format(date, "%Y"))]
fld_means[, date := paste0(year, "-01-01 00:00:00")]
fld_means[, date := as.POSIXct(date)]
slope <- TheilSen(fld_means, pollutant = "value", autocor = FALSE, plot = F, silent = T)
lm(value ~ year, data = fld_means)
mean_fld <- mean(fld_means$value, na.rm = T)
plot(fld_means$date, fld_means$value-mean_fld, type = "b", ylim = c(-30, 30))


