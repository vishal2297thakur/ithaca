# Creates project paths ----

source("source/main.R")
source("database/07_dataset_fnames_evap.R")

## Paths ----
### Output ----
PATH_SAVE_EVAP_TREND <- paste0(PATH_SAVE, "evap_trend/")
PATH_SAVE_EVAP_TREND_RAW <- paste0(PATH_SAVE, "evap_trend/raw/")
PATH_SAVE_EVAP_TREND_SPATIAL <- paste0(PATH_SAVE, "evap_trend/spatial/")
PATH_SAVE_EVAP_TREND_FIGURES <- paste0(PATH_SAVE, "evap_trend/figures/")
PATH_SAVE_EVAP_TREND_TABLES <- paste0(PATH_SAVE, "evap_trend/tables/")

dir.create(PATH_SAVE_EVAP_TREND, showWarnings = FALSE)
dir.create(PATH_SAVE_EVAP_TREND_RAW, showWarnings = FALSE)
dir.create(PATH_SAVE_EVAP_TREND_SPATIAL, showWarnings = FALSE)
dir.create(PATH_SAVE_EVAP_TREND_FIGURES, showWarnings = FALSE)
dir.create(PATH_SAVE_EVAP_TREND_TABLES, showWarnings = FALSE)

save(PATH_SAVE_EVAP_TREND, 
     PATH_SAVE_EVAP_TREND_RAW,
     PATH_SAVE_EVAP_TREND_SPATIAL,
     PATH_SAVE_EVAP_TREND_FIGURES,
     PATH_SAVE_EVAP_TREND_TABLES,
     file = paste0(PATH_SAVE_EVAP_TREND, "paths.Rdata"))
