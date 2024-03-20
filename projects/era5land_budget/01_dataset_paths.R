source('source/main.R')
source('source/era5land_budget.R')

library(pRecipe)

prec_all_fpaths <- c(list.files(path = PATH_PREC_SIM, full.names = TRUE),
              list.files(path = PATH_PREC_OBS, full.names = TRUE))
prec_datasets_fpaths <- unique(grep(paste(prec_dataset_names, collapse = "|"), 
                             prec_all_fpaths, value = TRUE))
prec_datasets_fpaths <- grep("land", prec_datasets_fpaths, value = TRUE)
prec_datasets_fpaths <- grep("monthly", prec_datasets_fpaths, value = TRUE)
prec_datasets_fpaths <- prec_datasets_fpaths[c(1, 4, 3, 7, 5, 6)]

evap_all_fpaths <- c(list.files(path = PATH_EVAP_SIM, full.names = TRUE),
              list.files(path = PATH_EVAP_OBS, full.names = TRUE))
evap_datasets_fpaths <- unique(grep(paste(evap_dataset_names, collapse = "|"), 
                             evap_all_fpaths, value = TRUE))
evap_datasets_fpaths <- grep("land", evap_datasets_fpaths, value = TRUE)
evap_datasets_fpaths <- grep("monthly", evap_datasets_fpaths, value = TRUE)
evap_datasets_fpaths <- evap_datasets_fpaths[c(1, 7, 3, 5)]

runoff_all_fpaths <- list.files(path = PATH_RUNOFF_SIM, full.names = TRUE)
runoff_datasets_fpaths <- unique(grep(paste(runoff_dataset_names, collapse = "|"), 
                                    runoff_all_fpaths, value = TRUE))
runoff_datasets_fpaths <- runoff_datasets_fpaths[c(1, 3, 2)]

save(prec_datasets_fpaths, 
     evap_datasets_fpaths,
     runoff_datasets_fpaths,
     file = paste0(PATH_SAVE_ERA5LAND_BUDGET, "dataset_fpaths.Rdata"))
