#Creates project paths

source("source/main.R")
source("database/06_dataset_fnames.R")

## Paths
### Output
PATH_SAVE_ERA5LAND_BUDGET <- paste0(PATH_SAVE, "era5land_budget/")
PATH_SAVE_ERA5LAND_BUDGET_RAW <- paste0(PATH_SAVE, "era5land_budget/raw/")
PATH_SAVE_ERA5LAND_BUDGET_SPATIAL <- paste0(PATH_SAVE, "era5land_budget/spatial/")
PATH_SAVE_ERA5LAND_BUDGET_FIGURES <- paste0(PATH_SAVE, "era5land_budget/figures/")
PATH_SAVE_ERA5LAND_BUDGET_TABLES <- paste0(PATH_SAVE, "era5land_budget/tables/")

dir.create(PATH_SAVE_ERA5LAND_BUDGET, showWarnings = FALSE)
dir.create(PATH_SAVE_ERA5LAND_BUDGET_RAW, showWarnings = FALSE)
dir.create(PATH_SAVE_ERA5LAND_BUDGET_SPATIAL, showWarnings = FALSE)
dir.create(PATH_SAVE_ERA5LAND_BUDGET_FIGURES, showWarnings = FALSE)
dir.create(PATH_SAVE_ERA5LAND_BUDGET_TABLES, showWarnings = FALSE)

save(PATH_SAVE_ERA5LAND_BUDGET, 
     PATH_SAVE_ERA5LAND_BUDGET_RAW,
     PATH_SAVE_ERA5LAND_BUDGET_SPATIAL,
     PATH_SAVE_ERA5LAND_BUDGET_FIGURES,
     PATH_SAVE_ERA5LAND_BUDGET_TABLES,
     file = paste0(PATH_SAVE_ERA5LAND_BUDGET, "paths.Rdata"))
