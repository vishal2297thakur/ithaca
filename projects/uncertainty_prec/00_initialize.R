# Listing data to be used in the project
source("source/uncertainty_prec.R")

## Data
PREC_NAMES_SHORT_2001_2019_FULL_RECORD <-  c("cmap", "cmorph", "cpc", "cru-ts",
                                             "em-earth", "era5", "era5-land",
                                             "fldas", "gpcc", "gpcp",
                                             "gpm-imerg", "gsmap", "jra55",
                                             "merra2", "mswep", "ncep-doe",
                                             "ncep-ncar", "persiann", "precl",
                                             "terraclimate")

PREC_NAMES_2001_2019_FULL_RECORD <- c(list.files(path = PATH_PREC_SIM,
                                                 full.names = TRUE),
                                      list.files(path = PATH_PREC_OBS,
                                                 full.names = TRUE))

PREC_NAMES_2001_2019_FULL_RECORD <- unique(grep(paste(PREC_NAMES_SHORT_2001_2019_FULL_RECORD,
                                                      collapse = "|"),
                                                PREC_NAMES_2001_2019_FULL_RECORD,
                                                value = TRUE))

PREC_NAMES_2001_2019_FULL_RECORD <- grep("land",
                                         PREC_NAMES_2001_2019_FULL_RECORD,
                                         value = TRUE)

PREC_NAMES_2001_2019_FULL_RECORD <- grep("monthly",
                                         PREC_NAMES_2001_2019_FULL_RECORD,
                                         value = TRUE)

## Save
save(PREC_NAMES_SHORT_2001_2019_FULL_RECORD,
     PREC_NAMES_2001_2019_FULL_RECORD,
     file = paste0(PATH_SAVE_UNCERTAINTY_PREC,
                   "prec_names_2001_2019_full_record.rda"))
