source("source/main.R")

# Packages 
packages <- c('gtools')
install.packages(setdiff(packages, rownames(installed.packages())))

# Paths
PATH_SAVE_BLUEPRINT <- paste0(PATH_SAVE, "blueprint/")
PATH_SAVE_BLUEPRINT_RAW <- paste0(PATH_SAVE, "blueprint/raw/")
PATH_SAVE_BLUEPRINT_SPATIAL <- paste0(PATH_SAVE, "blueprint/spatial/")
PATH_SAVE_BLUEPRINT_FIGURES <- paste0(PATH_SAVE, "blueprint/figures/")

PREC_FNAMES_2000_2019_KENYA <-  list.files(path = PATH_SAVE_BLUEPRINT_RAW, full.names = TRUE)
PREC_FNAMES_2000_2019_KENYA <-  grep("kenya_200001_201912", PREC_FNAMES_2000_2019_KENYA, value = TRUE)
PREC_FNAMES_SHORT_2000_2019_KENYA <-  PREC_FNAMES_SHORT_2000_2019

# Variables
n_datasets_2000_2019 <- length(PREC_FNAMES_2000_2019)

# Specify the lat/lon for the region of analysis 
study_area <- extent(PILOT_LON_MIN, 
                     PILOT_LON_MAX, 
                     PILOT_LAT_MIN, 
                     PILOT_LAT_MAX)

# Specify start/end for the period of analysis 
period_start <- as.Date("2000-01-01") 
period_end <- ITHACA_PERIOD_END
period_months <- interval(period_start, period_end) %/% months(1) + 1

# Bias
mid_cv_bias <- 0.8
low_cv_bias <- 0.5

# Functions

