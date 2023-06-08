source("source/main.R")

## Paths
### Input - Raw data 
PREC_FNAMES_SHORT_2000_2019_FULL_RECORD <-  c("era5", "jra55", "merra2", "ncep-doe", "ncep-ncar", "chirps", 
                                              "cmap", "cmorph", "cpc", "cru-ts", "em-earth", "gpcc", "gpcp", "gpm-imerg",
                                              "mswep", "persiann", "precl")
PREC_FNAMES_2000_2019_FULL_RECORD <- c(list.files(path = PATH_PREC_SIM, full.names = TRUE),
                                       list.files(path = PATH_PREC_OBS, full.names = TRUE))

PREC_FNAMES_2000_2019_FULL_RECORD <- unique(grep(paste(PREC_FNAMES_SHORT_2000_2019_FULL_RECORD, collapse = "|"), 
                                                 PREC_FNAMES_2000_2019_FULL_RECORD, value = TRUE))
PREC_FNAMES_2000_2019_FULL_RECORD <- grep("land", PREC_FNAMES_2000_2019_FULL_RECORD, value = TRUE)
PREC_FNAMES_2000_2019_FULL_RECORD <- grep("yearly", PREC_FNAMES_2000_2019_FULL_RECORD, value = TRUE)

### Output
PATH_SAVE_PARTITION_PREC <- paste0(PATH_SAVE, "partition_prec/")
PATH_SAVE_PARTITION_PREC_RAW <- paste0(PATH_SAVE, "partition_prec/raw/")
PATH_SAVE_PARTITION_PREC_SPATIAL <- paste0(PATH_SAVE, "partition_prec/spatial/")
PATH_SAVE_PARTITION_PREC_FIGURES <- paste0(PATH_SAVE, "partition_prec/figures/")
PATH_SAVE_PARTITION_PREC_TABLES <- paste0(PATH_SAVE, "partition_prec/tables/")

dir.create(PATH_SAVE_PARTITION_PREC)
dir.create(PATH_SAVE_PARTITION_PREC_RAW)
dir.create(PATH_SAVE_PARTITION_PREC_SPATIAL)
dir.create(PATH_SAVE_PARTITION_PREC_FIGURES)
dir.create(PATH_SAVE_PARTITION_PREC_TABLES)

save(PREC_FNAMES_2000_2019_FULL_RECORD, 
     PATH_SAVE_PARTITION_PREC, 
     PATH_SAVE_PARTITION_PREC_RAW,
     PATH_SAVE_PARTITION_PREC_SPATIAL,
     PATH_SAVE_PARTITION_PREC_FIGURES,
     PATH_SAVE_PARTITION_PREC_TABLES,
     file = paste0(PATH_SAVE_PARTITION_PREC, "paths.Rdata"))