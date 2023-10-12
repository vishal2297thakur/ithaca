source("source/main.R")

## Packages 
packages <- c('gtools', 'rnaturalearth', 'ggthemes', 'scales', 'ggpattern', "corrr")
install.packages(setdiff(packages, rownames(installed.packages())))

## Paths
load(paste0(PATH_SAVE, "/partition_prec/paths.Rdata"))
load(paste0(PATH_SAVE_PARTITION_PREC, "prec_names_2000_2019.Rdata"))

## Variables
MIN_N_DATASETS <- 10
n_datasets_2000_2019 <- length(PREC_FNAMES_2000_2019)

## Specify start/end for the period of analysis 
period_start <- as.Date("2000-01-01") 
period_end <- ITHACA_PERIOD_END
period_months <- interval(period_start, period_end) %/% months(1) + 1


