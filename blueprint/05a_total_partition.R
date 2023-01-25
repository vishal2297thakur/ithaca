## Read data 
prec_stats <- readRDS(paste0(path_save_blueprint, "prec_ensemble_stats.rds"))
prec_stats_mean[, sum(mean), quant]
