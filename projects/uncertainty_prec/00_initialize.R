source('source/uncertainty_prec.R')

## Paths
### Output
PATH_SAVE_UNCERTAINTY_PREC <- paste0(PATH_SAVE, 'uncertainty_prec/')
PATH_SAVE_UNCERTAINTY_PREC_RAW <- paste0(PATH_SAVE, 'uncertainty_prec/raw/')
PATH_SAVE_UNCERTAINTY_PREC_SPATIAL <- paste0(PATH_SAVE,
                                             'uncertainty_prec/spatial/')
PATH_SAVE_UNCERTAINTY_PREC_FIGURES <- paste0(PATH_SAVE,
                                             'uncertainty_prec/figures/')
PATH_SAVE_UNCERTAINTY_PREC_TABLES <- paste0(PATH_SAVE,
                                            'uncertainty_prec/tables/')

dir.create(PATH_SAVE_UNCERTAINTY_PREC, showWarnings = FALSE)
dir.create(PATH_SAVE_UNCERTAINTY_PREC_RAW, showWarnings = FALSE)
dir.create(PATH_SAVE_UNCERTAINTY_PREC_SPATIAL, showWarnings = FALSE)
dir.create(PATH_SAVE_UNCERTAINTY_PREC_FIGURES, showWarnings = FALSE)
dir.create(PATH_SAVE_UNCERTAINTY_PREC_TABLES, showWarnings = FALSE)

save(PATH_SAVE_UNCERTAINTY_PREC, 
     PATH_SAVE_UNCERTAINTY_PREC_RAW,
     PATH_SAVE_UNCERTAINTY_PREC_SPATIAL,
     PATH_SAVE_UNCERTAINTY_PREC_FIGURES,
     PATH_SAVE_UNCERTAINTY_PREC_TABLES,
     file = paste0(PATH_SAVE_UNCERTAINTY_PREC, 'uncertainty_prec_paths.Rdata'))

## Period 2000-2019
PREC_FNAMES_SHORT_2000_2019 <-  c('chirps', 'cmap', 'cmorph', 'cpc', 'cru-ts',
                                  'em-earth', 'era5', 'gpcc', 'gpcp',
                                  'gpm-imerg', 'jra55', 'merra2', 'mswep',
                                  'ncep-doe', 'ncep-ncar', 'persiann', 'precl')
PREC_FNAMES_2000_2019 <- c(list.files(path = PATH_PREC_SIM, full.names = TRUE),
                                       list.files(path = PATH_PREC_OBS,
                                                  full.names = TRUE))

PREC_FNAMES_2000_2019 <- unique(grep(paste(PREC_FNAMES_SHORT_2000_2019,
                                           collapse = '|'),
                                     PREC_FNAMES_2000_2019, value = TRUE))
PREC_FNAMES_2000_2019 <- grep('land', PREC_FNAMES_2000_2019, value = TRUE)
PREC_FNAMES_2000_2019 <- grep('monthly', PREC_FNAMES_2000_2019, value = TRUE)

save(PREC_FNAMES_2000_2019, file = paste0(PATH_SAVE_UNCERTAINTY_PREC,
                                          'prec_fnames_2000_2019.Rdata'))
