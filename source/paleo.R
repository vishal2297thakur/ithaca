source("source/main.R")

PATH_SAVE_PALEO <- paste0(PATH_SAVE, "paleo/")
PATH_SAVE_PALEO_RAW <- paste0(PATH_SAVE, "paleo/raw/")
PATH_SAVE_PALEO_SPATIAL <- paste0(PATH_SAVE, "paleo/spatial/")
PATH_SAVE_PALEO_FIGURES <- paste0(PATH_SAVE, "paleo/figures/")
PATH_SAVE_PALEO_TABLES <- paste0(PATH_SAVE, "paleo/tables/")
PATH_SAVE_PALEO_FIGURES_SUPP <- paste0(PATH_SAVE, "paleo/figures/SUPPLEMENT/")
PATH_SAVE_PALEO_FIGURES_EXPLORE <- paste0(PATH_SAVE, "paleo/figures/Exploration/")
PATH_SAVE_PALEO_FIGURES_MAIN <- paste0(PATH_SAVE, "paleo/figures/MAIN/")

PREC_DATASETS <- factor(c("cru-ts", "em-earth", "era5-land", "jra55", "gpcc", "precl"))
PREC_FNAMES <- c("/mnt/shared/data/obs/precip/raw/cru-ts-v4-07_tp_mm_land_190101_202212_025_monthly.nc",
                 "/mnt/shared/data/obs/precip/raw/em-earth_tp_mm_land_195001_201912_025_monthly.nc",
                 "/mnt/shared/data/sim/precip/raw/era5-land_tp_mm_land_195001_202112_025_monthly.nc",
                 "/mnt/shared/data/sim/precip/raw/jra55_tp_mm_land_195801_202309_025_monthly.nc",
                 "/mnt/shared/data/obs/precip/raw/gpcc-v2020_tp_mm_land_189101_201912_025_monthly.nc",
                 "/mnt/shared/data/obs/precip/raw/precl_tp_mm_land_194801_202208_025_monthly.nc")

