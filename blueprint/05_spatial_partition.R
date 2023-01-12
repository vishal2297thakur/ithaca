## Read data 
prec_era5_kenya <- raster(paste0(path_save_blueprint, "prec_era5.nc"))
prec_gpcc_kenya <- raster(paste0(path_save_blueprint, "prec_gpcc.nc"))
prec_em_kenya <- raster(paste0(path_save_blueprint, "prec_em.nc"))
prec_gpcp_kenya <- raster(paste0(path_save_blueprint, "prec_gpcp.nc"))
prec_mswep_kenya <- raster(paste0(path_save_blueprint, "prec_mswep.nc"))
prec_gpm_kenya <- raster(paste0(path_save_blueprint, "prec_gpm.nc"))

prec_stats_mean <- readRDS(paste0(path_save_blueprint, "prec_stats_mean.rds"))
prec_stats_mean[, sum(mean), quant]
