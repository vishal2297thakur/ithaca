# Reading and subsetting data for the specified period

source("source/uncertainty_prec.R")

load(paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_names_2000_2019_full_record.rda"))

registerDoParallel(cores = N_CORES - 1)
n_datasets_2000_2019 <- length(PREC_NAMES_2000_2019_FULL_RECORD_MONTH)
start_year <- year(PERIOD_START)
end_year <- year(PERIOD_END)

foreach(data_idx = 1:n_datasets_2000_2019) %dopar% {
  result <- subset_time(PREC_NAMES_2000_2019_FULL_RECORD_MONTH[data_idx],
                            c(start_year, end_year))
  short_name <- sub(".*/([^_/]*)_.*", "\\1",
                    PREC_NAMES_2000_2019_FULL_RECORD_MONTH[data_idx])
  nc_out <- paste0(PATH_SAVE_UNCERTAINTY_PREC_RAW, short_name,
                   "_tp_mm_land_200001_201912_025_monthly.nc")
  save_nc(result, nc_out)
}

n_datasets_2000_2019 <- length(PREC_NAMES_2000_2019_FULL_RECORD_YEARS)
start_year <- year(PERIOD_START)
end_year <- year(PERIOD_END)

foreach(data_idx = 1:n_datasets_2000_2019) %dopar% {
  result <- subset_time(PREC_NAMES_2000_2019_FULL_RECORD_YEARS[data_idx],
                        c(start_year, end_year))
  short_name <- sub(".*/([^_/]*)_.*", "\\1",
                    PREC_NAMES_2000_2019_FULL_RECORD_YEARS[data_idx])
  nc_out <- paste0(PATH_SAVE_UNCERTAINTY_PREC_RAW, short_name,
                   "_tp_mm_land_200001_201912_025_yearly.nc")
  save_nc(result, nc_out)
}


PREC_NAMES_2000_2019_MONTH <- list.files(path = PATH_SAVE_UNCERTAINTY_PREC_RAW,
                                         pattern = "*monthly.nc",
                                         full.names = TRUE)

PREC_NAMES_2000_2019_YEARS <- list.files(path = PATH_SAVE_UNCERTAINTY_PREC_RAW,
                                         pattern = "*yearly.nc",
                                         full.names = TRUE)

PREC_NAMES_SHORT_2000_2019 <- sub(".*/([^_/]*)_.*", "\\1",
                                  PREC_NAMES_2000_2019_MONTH)

save(PREC_NAMES_SHORT_2000_2019, PREC_NAMES_2000_2019_MONTH,
     PREC_NAMES_2000_2019_YEARS,
     file = paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_names_2000_2019.rda"))
