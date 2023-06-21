source('source/uncertainty_prec.R')

## Paths
## Period 2000-2019
PREC_NAMES_SHORT_2000_2019_FULL_RECORD <-  c('chirps', 'cmap', 'cmorph', 'cpc',
                                              'cru-ts', 'em-earth', 'era5',
                                              'gpcc', 'gpcp', 'gpm-imerg',
                                              'jra55', 'merra2', 'mswep',
                                              'ncep-doe', 'ncep-ncar',
                                              'persiann', 'precl')
PREC_NAMES_2000_2019_FULL_RECORD <- c(list.files(path = PATH_PREC_SIM,
                                                  full.names = TRUE),
                                       list.files(path = PATH_PREC_OBS,
                                                  full.names = TRUE))

PREC_NAMES_2000_2019_FULL_RECORD <- unique(grep(paste(PREC_NAMES_SHORT_2000_2019_FULL_RECORD,
                                           collapse = '|'),
                                     PREC_NAMES_2000_2019_FULL_RECORD,
                                     value = TRUE))
PREC_NAMES_2000_2019_FULL_RECORD <- grep('land',
                                          PREC_NAMES_2000_2019_FULL_RECORD,
                                          value = TRUE)
PREC_NAMES_2000_2019_FULL_RECORD <- grep('monthly',
                                          PREC_NAMES_2000_2019_FULL_RECORD,
                                          value = TRUE)

save(PREC_NAMES_SHORT_2000_2019_FULL_RECORD, PREC_NAMES_2000_2019_FULL_RECORD,
     file = paste0(PATH_SAVE_UNCERTAINTY_PREC,
                   'prec_names_2000_2019_full_record.rda'))
