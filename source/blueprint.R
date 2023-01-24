source("source/main.R")

# Packages 

# Paths
path_save_blueprint <- paste0(path_save, "blueprint/")
prec_fnames_2000_2019_kenya <-  list.files(path = path_save_blueprint, full.names = TRUE)
prec_fnames_2000_2019_kenya <-  grep("kenya_200006_201912", prec_fnames_2000_2019_kenya, value = TRUE)
prec_fnames_short_2000_2019_kenya <-  prec_fnames_short_2000_2019

# Variables
n_datasets_2000_2019 <- length(prec_fnames_2000_2019)
#n_datasets_1980_2019 <- length(prec_fnames_1980_2019)
#n_datasets_1960_2019 <- length(prec_fnames_1960_2019)
n_variables <- 1

# Specify the lat/lon for the region of analysis 
study_area <- extent(PILOT_LON_MIN, 
                     PILOT_LON_MAX, 
                     PILOT_LAT_MIN, 
                     PILOT_LAT_MAX)

# Specify start/end for the period of analysis 
period_start <- as.Date("2000-06-01") 
period_end <- as.Date("2019-12-31") 
period_months <- interval(period_start, period_end) %/% months(1) + 1

period_start_change <- PERIOD_START
period_end_change <- PERIOD_END
period_months_change <- interval(period_start, period_end) %/% months(1) + 1

# Bias
mid_cv_bias <- 0.8
low_cv_bias <- 0.5

# Functions

