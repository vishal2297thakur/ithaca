## packages
# generic
library(data.table)

# plotting
library(ggplot2)
library(ggpubr)

# geospatial
library(pRecipe)
library(raster)

# parallel
library(doParallel)

## Paths
PATH_DATA <- '~/shared/data/'
PATH_DATA_REVIEW <- '~/shared/data_review/'
PATH_SAVE <- '~/shared/data_projects/ithaca/'
PATH_PREC_SIM <- paste0(PATH_DATA, 'sim/precip/raw/')
PATH_PREC_OBS <- paste0(PATH_DATA, 'obs/precip/raw/')

## Datasets
PREC_GLOBAL_DATASETS <- c('20cr', 'chirps', 'cmap', 'cmorph', 'cpc', 'cru-ts',
                          'era20c', 'era5', 'em-earth', 'fldas', 'ghcn',
                          'gldas-clsm', 'gldas-noah', 'gldas-vic', 'gpcc',
                          'gpcp', 'gpm-imerg', 'jra55', 'merra2', 'mswep',
                          'ncep-doe', 'ncep-ncar', 'persiann', 'precl',
                          'terraclimate', 'trmm-3b43', 'udel')

# Types
PREC_DATASETS_OBS <- c('cpc', 'cru-ts', 'em-earth', 'ghcn', 'gpcc', 'precl',
                       'udel')
PREC_DATASETS_REANAL <- c('20cr', 'era20c', 'era5', 'jra55', 'merra2',
                          'ncep-doe', 'ncep-ncar')
PREC_DATASETS_REMOTE <- c('chirps', 'cmap', 'cmorph','gpcp', 'gpm-imerg',
                          'mswep', 'persiann', 'trmm-3b43')
PREC_DATASETS_HYDROL <- c('fldas', 'gldas-clsm', 'gldas-noah', 'gldas-vic',
                          'terraclimate')

## Constants
# Time
PERIOD_START <- as.Date('2000-01-01')
PERIOD_END <- as.Date('2019-12-31')

# Space
GLOBAL_AREA <- 1.345883e+14 

## Variable names
PREC_NAME <- 'prec'

PREC_NAME_SHORT <- 'tp'

## Parallelization
N_CORES <- detectCores()

## Other
M2_TO_KM2 <- 10 ^ (-6)
MM_TO_M <- 10 ^ (-3)
MM_TO_KM <- 10 ^ (-6)

