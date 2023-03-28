## Paths & files
### Input
FNAME_PREC_EM_EARTH <- "/home/rstudio/shared/data/obs/precip/raw/em-earth_tp_mm_land_195001_201912_025_monthly.nc"
FNAME_SPEI_PHYDA <- "/home/rstudio/shared/data_downloads/paleo_data/PHYDA/PHYDA_spei_mn_global_1_2000_025_yearly.nc"
FNAME_TEMP_PHYDA <- "/home/rstudio/shared/data_downloads/paleo_data/PHYDA/PHYDA_tas_mn_global_1_2000_025_yearly.nc"

### Output
PATH_SAVE_EUROPE <- paste0(PATH_SAVE, "europe/")
PATH_SAVE_EUROPE_RAW <- paste0(PATH_SAVE, "europe/raw")
PATH_SAVE_EUROPE_SPATIAL <- paste0(PATH_SAVE, "europe/spatial/")
PATH_SAVE_EUROPE_FIGURES <- paste0(PATH_SAVE, "europe/figures/")
PATH_SAVE_EUROPE_TABLES <- paste0(PATH_SAVE, "europe/tables/")

## Case study
### Time
period_start <- as.Date("1990-01-01") 
period_end <- ITHACA_PERIOD_END

### Space
lat_max <- 5 
lat_min <- -5.25
lon_max <- 44.25
lon_min <- 33.75 

study_area <- extent(lon_min, 
                     lon_max, 
                     lat_min, 
                     lat_max)