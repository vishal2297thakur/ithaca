# Soil moisture based index using terraclimate ----
source('source/partition_evap.R')
source('source/geo_functions.R')
source('source/cdo_functions.R')

# terraclimate ----
## Load data ----
fnames_nc <- list.files(path = paste0(PATH_DATA, "sim/soilmoisture/raw/"), pattern = ".nc$", 
                        full.names = T)
fnames_product <- fnames_nc[grep("terraclimate", fnames_nc)]
fname <- fnames_product[grep("monthly", fnames_product)]
cdo_sinfo_fnc(fname)
cdo_info_fnc(fname)


## Analysis ----
### Convert data to yearly and fix data type and date ----
cmd_cdo <- paste0("cdo -b 32 yearmonmean ", fname," " ,paste0(PATH_DATA, "sim/soilmoisture/raw/terraclimate_sm_mm_global-land_195801_202112_025_yearly_temp.nc"))
system(cmd_cdo)
cmd_cdo <- paste0("cdo showdate ", PATH_DATA, "sim/soilmoisture/raw/terraclimate_sm_mm_global-land_195801_202112_025_yearly_temp.nc ")
system(cmd_cdo)
cmd_cdo <- paste0("cdo setmon,1 ", PATH_DATA, "sim/soilmoisture/raw/terraclimate_sm_mm_global-land_195801_202112_025_yearly_temp.nc ", PATH_DATA, "sim/soilmoisture/raw/terraclimate_sm_mm_global-land_195801_202112_025_yearly_temp2.nc")
system(cmd_cdo)
cmd_cdo <- paste0("cdo showdate ", PATH_DATA, "sim/soilmoisture/raw/terraclimate_sm_mm_global-land_195801_202112_025_yearly_temp2.nc ")
system(cmd_cdo)
cmd_cdo <- paste0("cdo setday,1 ", PATH_DATA, "sim/soilmoisture/raw/terraclimate_sm_mm_global-land_195801_202112_025_yearly_temp2.nc ", PATH_DATA, "sim/soilmoisture/raw/terraclimate_sm_mm_global-land_195801_202112_025_yearly_temp3.nc")
system(cmd_cdo)
cmd_cdo <- paste0("cdo settime,00:00:00 ", PATH_DATA, "sim/soilmoisture/raw/terraclimate_sm_mm_global-land_195801_202112_025_yearly_temp3.nc ", PATH_DATA, "sim/soilmoisture/raw/terraclimate_sm_mm_global-land_195801_202112_025_yearly.nc")
system(cmd_cdo)
cmd_cdo <- paste0("cdo showdate ", PATH_DATA, "sim/soilmoisture/raw/terraclimate_sm_mm_global-land_195801_202112_025_yearly.nc ")
system(cmd_cdo)
cdo_info_fnc(paste0(PATH_DATA, "sim/soilmoisture/raw/terraclimate_sm_mm_global-land_195801_202112_025_yearly.nc"))

### Remove temporary files ----
system(paste0("rm ",PATH_DATA, "sim/soilmoisture/raw/terraclimate_sm_mm_global-land_195801_202112_025_yearly_temp3.nc"))
system(paste0("rm ",PATH_DATA, "sim/soilmoisture/raw/terraclimate_sm_mm_global-land_195801_202112_025_yearly_temp2.nc"))
system(paste0("rm ",PATH_DATA, "sim/soilmoisture/raw/terraclimate_sm_mm_global-land_195801_202112_025_yearly_temp.nc"))

### reload data ----
fnames_nc <- list.files(path = paste0(PATH_DATA, "sim/soilmoisture/raw/"), pattern = ".nc$", 
                        full.names = T)
fnames_product <- fnames_nc[grep("terraclimate", fnames_nc)]
fname <- fnames_product[grep("yearly", fnames_product)]


### Select time step ----
period_start <- as.Date("2000-01-01") 
period_end <- as.Date("2019-01-01") 

data <- brick(fname)
start_time <- which(data@z$Date == period_start)
end_time <- which(data@z$Date == period_end)

output_fname <- paste0(PATH_DATA, "sim/soilmoisture/processed/terraclimate_sm_mm_global-land_200001_201912_025_yearly.nc")
cdo_seltimestep_fnc(fname, outputfile_name = paste0(PATH_DATA, "sim/soilmoisture/processed/terraclimate_sm_mm_global-land_200001_201912_025_yearly.nc"), 
                    start_time = start_time, end_time = end_time)


### Calculate mean and standard deviation ----
# Mean
cdo_timmean_fnc(output_fname, outputfile_name = paste0(PATH_DATA, "sim/soilmoisture/processed/terraclimate_sm_mm_global-land_200001_201912_025_yearly_mean.nc"))
# Standard deviation
cdo_timstd_fnc(output_fname, outputfile_name = paste0(PATH_DATA, "sim/soilmoisture/processed/terraclimate_sm_mm_global-land_200001_201912_025_yearly_std.nc"))


### Calculate SSI ----
cmd_cdo <- paste0("cdo sub ", PATH_DATA, "sim/soilmoisture/processed/terraclimate_sm_mm_global-land_200001_201912_025_yearly.nc ", 
                  PATH_DATA, "sim/soilmoisture/processed/terraclimate_sm_mm_global-land_200001_201912_025_yearly_mean.nc ", 
                  PATH_DATA, "sim/soilmoisture/processed/terraclimate_sm_mm_global-land_200001_201912_025_yearly-mean.nc")
system(cmd_cdo)

cmd_cdo <- paste0("cdo div ",PATH_DATA, "sim/soilmoisture/processed/terraclimate_sm_mm_global-land_200001_201912_025_yearly-mean.nc ",
                  PATH_DATA, "sim/soilmoisture/processed/terraclimate_sm_mm_global-land_200001_201912_025_yearly_std.nc ",
                  PATH_DATA, "sim/soilmoisture/processed/terraclimate_ssi_global-land_200001_201912_025_yearly.nc")
system(cmd_cdo)

### Remove temporary files ----
system(paste0("rm ",PATH_DATA, "sim/soilmoisture/processed/terraclimate_sm_mm_global-land_200001_201912_025_yearly_mean.nc"))
system(paste0("rm ",PATH_DATA, "sim/soilmoisture/processed/terraclimate_sm_mm_global-land_200001_201912_025_yearly_std.nc"))
system(paste0("rm ",PATH_DATA, "sim/soilmoisture/processed/terraclimate_sm_mm_global-land_200001_201912_025_yearly-mean.nc"))

## Save data as data.table ----
data_ssi <- brick(paste0(PATH_DATA, "sim/soilmoisture/processed/terraclimate_ssi_global-land_200001_201912_025_yearly.nc"))
data_ssi_dt <- brick_to_dt(data_ssi)
saveRDS(data_ssi_dt, paste0(PATH_SAVE_PARTITION_EVAP, "ssi_200001_201912_terraclimate.rds"))

## plot data ----

ggplot(data_ssi_dt[time == as.Date("2003-01-01")])+
  geom_tile(aes(x = x, y = y, fill = value, col = value))+
  theme_bw()


