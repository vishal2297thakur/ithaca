source("source/main.R")

## Packages 
packages <- c('gtools', 'rnaturalearth', 'kohonen', 'ggthemes', 'scales', 'ggpattern', "corrr")
install.packages(setdiff(packages, rownames(installed.packages())))

## Paths
### Input - Raw data 
EVAP_FNAMES_SHORT_2000_2019_FULL_RECORD <-  c("bess","camele","era5-land","etmonitor","etsynthesis","fldas", "gldas-clsm","gldas-noah", 
                                              "gldas-vic","gleam", "jra55", "merra2","mod16a", "terraclimate")
EVAP_FNAMES_2000_2019_FULL_RECORD <- c(list.files(path = PATH_EVAP_SIM, full.names = TRUE,pattern = "*_e_mm*"))

EVAP_FNAMES_2000_2019_FULL_RECORD <- unique(grep(paste(EVAP_FNAMES_SHORT_2000_2019_FULL_RECORD, collapse = "|"), 
                                                 EVAP_FNAMES_2000_2019_FULL_RECORD, value = TRUE))

EVAP_FNAMES_2000_2019_FULL_RECORD <- grep("land", EVAP_FNAMES_2000_2019_FULL_RECORD, value = TRUE)
EVAP_FNAMES_2000_2019_FULL_RECORD <- grep("yearly", EVAP_FNAMES_2000_2019_FULL_RECORD, value = TRUE)

### Output
PATH_SAVE_PARTITION_EVAP <- paste0(PATH_SAVE, "partition_evap/")
PATH_SAVE_PARTITION_EVAP_RAW <- paste0(PATH_SAVE, "partition_evap/raw/")
PATH_SAVE_PARTITION_EVAP_SPATIAL <- paste0(PATH_SAVE, "partition_evap/spatial/")
PATH_SAVE_PARTITION_EVAP_FIGURES <- paste0(PATH_SAVE, "partition_evap/figures/")
PATH_SAVE_PARTITION_EVAP_TABLES <- paste0(PATH_SAVE, "partition_evap/tables/")

### Project data
EVAP_FNAMES_2000_2019 <-  list.files(path = PATH_SAVE_PARTITION_EVAP_RAW, full.names = TRUE)
dummy <- strsplit(EVAP_FNAMES_2000_2019, split = '//')
dummy <- sapply(dummy, "[[", 2)
dummy <- strsplit(dummy, split = '_')
EVAP_FNAMES_SHORT_2000_2019 <- sapply(dummy, "[[", 1)

## Variables
MIN_N_DATASETS <- 13
#n_datasets_2000_2019 <- length(EVAP_FNAMES_SHORT_2000_2019)
n_datasets_2000_2019 <- 14

## Specify start/end for the period of analysis 
period_start <- as.Date("2000-01-01") 
period_end <- ITHACA_PERIOD_END
period_months <- interval(period_start, period_end) %/% months(1) + 1

#global space
global_area_evap <- 125803654946773
