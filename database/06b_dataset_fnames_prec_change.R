source("source/main.R")

# Precipitation

## Period 2000-2019
PREC_FNAMES_SHORT_2000_2019_FULL_RECORD <-  c("era5-land", "jra55", "merra2", "cmap", 
                                              "cru-ts", "fldas", "gpcc", "gpcp", 
                                              "gpm-imerg", "gsmap", "mswep", "precl")
PREC_FNAMES_2000_2019_FULL_RECORD <- c(list.files(path = PATH_PREC_SIM, full.names = TRUE),
                                       list.files(path = PATH_PREC_OBS, full.names = TRUE))

PREC_FNAMES_2000_2019_FULL_RECORD <- unique(grep(paste(PREC_FNAMES_SHORT_2000_2019_FULL_RECORD, collapse = "|"), 
                                                 PREC_FNAMES_2000_2019_FULL_RECORD, value = TRUE))
PREC_FNAMES_2000_2019_FULL_RECORD <- grep("land", PREC_FNAMES_2000_2019_FULL_RECORD, value = TRUE)
PREC_FNAMES_2000_2019_FULL_RECORD <- grep("yearly", PREC_FNAMES_2000_2019_FULL_RECORD, value = TRUE)

save(PREC_FNAMES_2000_2019_FULL_RECORD, PREC_FNAMES_SHORT_2000_2019_FULL_RECORD,
     file = "~/shared/data_projects/ithaca/misc/prec_fnames_2000_2019_full_record_prec_change.Rdata")
