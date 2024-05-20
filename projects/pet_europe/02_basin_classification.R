################################################
# Basin classification
######################################################

#Required library
library(data.table)
library(fst)

FILE_PATH <- "~/shared/data_projects/ithaca/pet_europe/data/"
SAVE_PATH <- "~/shared/data_projects/ithaca/pet_europe/data/"

#loading data 
monthly_dt <- read_fst(paste0(FILE_PATH,"aet_pre_pet_q_tws_twsc.fst"), as.data.table = TRUE)
monthly_dt[, YEAR := year(date)]

#computing monthly to annual scale
yearly_dt <- monthly_dt[YEAR >= 1980 & YEAR <= 2019, 
                        .(value = sum(value, na.rm = TRUE)), by = .(basin, pet_method, variable, YEAR)]

#Getting required variable PET, AET, P
yearly_dt_dcast <- dcast(yearly_dt, basin + pet_method + YEAR ~ variable)
subset_yearly_dt  <- yearly_dt_dcast[, c("basin", "pet_method", "YEAR", "pet", "pre", "aet")] 

#Removing loaded variables (not required)
rm(monthly_dt, yearly_dt, yearly_dt_dcast)

#Computing aridity and evaporative index
aridity_dt <- subset_yearly_dt[, lapply(.SD, mean, na.rm = TRUE), by = .(basin,pet_method), .SDcols = c("pre", "pet", "aet")
                  ][,aridity_idx := pet/pre]

#Basin classification based on aridity index 
aridity_dt[, basin_type := fifelse(aridity_idx > 1 , "water_limited","energy_limited" )
           ][basin_type == "energy_limited", basin_type_1 := fifelse(sum(length(basin_type)) == length(unique(aridity_dt$pet_method) ), "energy_limited","mixed"), by = .(basin)
      ][basin_type == "water_limited", basin_type_1 := fifelse(sum(length(basin_type)) == length(unique(aridity_dt$pet_method) ), "water_limited","mixed"), by = .(basin)]

#Subsetting basin and basin type columns 
subset_aridity_dt <- unique(aridity_dt[, c("basin","basin_type_1")], by = "basin")
setnames(subset_aridity_dt, "basin_type_1", "basin_type")

#saving data
write_fst(subset_aridity_dt, paste0(SAVE_PATH, "basin_classification.fst"), compress = 50, uniform_encoding = TRUE)





