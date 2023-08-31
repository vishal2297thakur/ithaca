source("source/main.R")

## Paths
load(paste0(PATH_SAVE, "era5land_budget/paths.Rdata"))

## Variables
prec_dataset_names <- c('era5', 'terraclimate', 'gldas-noah', 'gpcc', 'cru-ts', 'em-earth')
evap_dataset_names <- c('era5-land', 'terraclimate', 'gldas-noah', 'gleam')

## Specify start/end for the period of analysis 
period_start <- as.Date("2000-01-01") 
period_end <- ITHACA_PERIOD_END
period_months <- interval(period_start, period_end) %/% months(1) + 1


