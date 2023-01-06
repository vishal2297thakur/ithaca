### Reading and subsetting data for the specified region and period

source('source/blueprint.R')
source('source/geo_functions.R')

fname_prec_era5 <- list.files(path = path_prec_sim, full.names = T, pattern = "era5_tp*") 
fname_prec_gpcc <- list.files(path = path_prec_obs, full.names = T, pattern = "gpcc_tp*") 
fname_prec_em <- list.files(path = path_prec_obs, full.names = T, pattern = "em-earth_tp*")
fname_prec_gpcp <- list.files(path = path_prec_obs, full.names = T, pattern = "gpcp_tp*")
fname_prec_mswep <- list.files(path = path_prec_obs, full.names = T, pattern = "mswep_tp*")
fname_prec_gpm <- list.files(path = path_prec_obs, full.names = T, pattern = "gpm-imerg_tp*")

## Read and subset data
prec_era5 <- brick(fname_prec_era5)
prec_era5_kenya <- crop_space_time(prec_era5, period_start, period_end, study_area) 
rm(prec_era5); gc()

prec_gpcc <- brick(fname_prec_gpcc)
prec_gpcc_kenya <- crop_space_time(prec_gpcc, period_start, period_end, study_area) 
rm(prec_gpcp); gc()

prec_em <- brick(fname_prec_em)
prec_em_kenya <- crop_space_time(prec_em, period_start, period_end, study_area) 
rm(prec_em); gc()

prec_gpcp <- brick(fname_prec_gpcp)
prec_gpcp_kenya <- crop_space_time(prec_gpcp, period_start, period_end, study_area) 
rm(prec_gpcp); gc()

prec_mswep <- brick(fname_prec_mswep)
prec_mswep_kenya <- crop_space_time(prec_mswep, period_start, period_end, study_area) 
rm(prec_mswep); gc()

prec_gpm <- brick(fname_prec_gpm)
prec_gpm_kenya <- crop_space_time(prec_gpm, period_start, period_end, study_area) 
rm(prec_gpm); gc()

## Save data for further use
saveRDS(prec_era5_kenya, paste0(path_save_blueprint, "prec_era5.rds"))
saveRDS(prec_gpcc_kenya, paste0(path_save_blueprint, "prec_gpcc.rds"))
saveRDS(prec_em_kenya, paste0(path_save_blueprint, "prec_em.rds"))
saveRDS(prec_gpcp_kenya, paste0(path_save_blueprint, "prec_gpcp.rds"))
saveRDS(prec_mswep_kenya, paste0(path_save_blueprint, "prec_mswep.rds"))
saveRDS(prec_gpm_kenya, paste0(path_save_blueprint, "prec_gpm.rds"))
