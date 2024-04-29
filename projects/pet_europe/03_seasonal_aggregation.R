##########################################################
# Seasonal computation from the monthly data  
#########################################################
# required library 
library(data.table)
library(fst)
library(lubridate)

FILE_PATH <- "~/shared/data_projects/ithaca/pet_europe/data/"
SAVE_PATH <- "~/shared/data_projects/ithaca/pet_europe/data/"

monthly_dt <- read_fst(paste0(FILE_PATH, "aet_pre_pet_q_tws_twsc.fst"), as.data.table = TRUE)

# Adding seasonal column to the datatable 
monthly_dt[, `:=`(YEAR = year(date), MONTH = month(date))
           ][, season_year := ifelse(MONTH == 12, YEAR +1, YEAR)
             ][MONTH == 12 | MONTH <= 2, season := "DJF"
               ][MONTH >= 3 & MONTH <= 5, season := "MAM"
                 ][MONTH >= 6 & MONTH <= 8, season := "JJA"
                   ][MONTH >= 9 & MONTH <= 11, season := "SON"]

# Seasonal aggregation at each basin 
seasonal_dt <- monthly_dt[, .(seasonal_value = sum(value, na.rm = TRUE)), 
                          by = .(basin, pet_method, variable, season_year, season)]

# Averaging for TWS (total water storage)
seasonal_dt[variable == "tws", seasonal_value := seasonal_value/3]

# Svaing data 
write_fst(seasonal_dt, paste0(SAVE_PATH, "aet_pre_pet_q_tws_twsc_seasonal.fst"))




