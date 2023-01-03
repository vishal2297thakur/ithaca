source("source/main.R")

# Packages 

# Paths
path_prec <- paste0(path_data,"sim/precip/raw/")
path_evap <- paste0(path_data,"sim/evap/raw/")
path_save_kenya <- paste0(path_save,"example_kenya/")

# Variables
n_datasets <- 2
n_variables <- 2

# Specify the lat/lon for the region of analysis | In this example we are using the constants from the source file main.R
study_area <- extent(PILOT_LON_MIN, 
                   PILOT_LON_MAX, 
                   PILOT_LAT_MIN, 
                   PILOT_LAT_MAX)

# Specify start/end for the period of analysis 
period_months <- interval(PERIOD_START, PERIOD_END) %/% months(1) + 1

## Functions

