source('source/europe.R')

prec_em_earth <- brick(paste0(PATH_SAVE_EUROPE_RAW, "/em-earth_tp_mm_europe_199001_201912_025_monthly.nc"))

spei_phyda <- brick(paste0(PATH_SAVE_EUROPE_RAW, "/PHYDA_spei_europe_101_201912_025_monthly.nc"))



