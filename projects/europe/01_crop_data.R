# Reading and subsetting data for the specified region and period
source('source/europe.R')
source('source/geo_functions.R')

prec_em_earth <- brick(FNAME_PREC_EM_EARTH)
spei_phyda <- brick(FNAME_SPEI_PHYDA)
temp_phyda <- brick(FNAME_TEMP_PHYDA)

prec_case_study <- crop_space_time(prec_em_earth,
                                   period_start, period_end, 
                                   study_area)
name_nc <- paste0(PATH_SAVE_EUROPE_RAW, "/em-earth_tp_mm_europe_199001_201912_025_monthly.nc")
save_nc(prec_case_study, name_nc)

spei_case_study <- crop(spei_phyda, study_area)
name_nc <- paste0(PATH_SAVE_EUROPE_RAW, "/PHYDA_spei_europe_101_201912_025_yearly.nc")
save_nc(spei_case_study, name_nc)

temp_case_study <- crop(temp_phyda, study_area)
name_nc <- paste0(PATH_SAVE_EUROPE_RAW, "/PHYDA_temp_europe_101_201912_025_yearly.nc")
save_nc(temp_case_study, name_nc)
