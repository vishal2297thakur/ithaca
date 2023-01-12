source("source/main.R")

# Packages 

# Paths
path_prec_sim <- paste0(path_data, "sim/precip/raw/")
path_prec_obs <- paste0(path_data, "obs/precip/raw/")
path_evap_sim <- paste0(path_data, "sim/evap/raw/")
path_evap_obs <- paste0(path_data, "obs/evap/raw/")
path_save_blueprint <- paste0(path_save, "blueprint/")

# Variables
n_datasets <- 6
n_variables <- 2

# Specify the lat/lon for the region of analysis 
study_area <- extent(PILOT_LON_MIN, 
                     PILOT_LON_MAX, 
                     PILOT_LAT_MIN, 
                     PILOT_LAT_MAX)

# Specify start/end for the period of analysis 
period_start <- as.Date("2001-01-01") 
period_end <- as.Date("2019-12-31") 
period_months <- interval(period_start, period_end) %/% months(1) + 1

period_start_change <- PERIOD_START
period_end_change <- PERIOD_END
period_months_change <- interval(period_start, period_end) %/% months(1) + 1

# Bias
mid_cv_bias <- 0.8
low_cv_bias <- 0.5

# Functions

