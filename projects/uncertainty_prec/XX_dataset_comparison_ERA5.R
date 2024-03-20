source('source/partition_prec.R')
source('source/graphics.R')

library(corrr)
library(openair)

## Load data
prec_annual <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_global_annual_mean.rds"))
prec_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_datasets.rds"))
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))

## Variables
data_for_cor_temporal <- dcast(prec_annual, year ~ dataset, value.var = 'prec_mean')

taylor_obs_era5 <- prec_annual[dataset == 'era5', .(year, obs = prec_mean)]
taylor_obs_cru <- prec_annual[dataset == 'cru-ts', .(year, obs = prec_mean)]
data_for_taylor_era5 <- merge(taylor_obs_era5, prec_annual[dataset != 'era5'], by = "year")
data_for_taylor_cru <- merge(taylor_obs_cru, prec_annual[dataset != 'era5'], by = "year")
setnames(data_for_taylor_era5, 'prec_mean', 'mod') 
setnames(data_for_taylor_cru, 'prec_mean', 'mod') 
data_for_taylor_era5 <- data_for_taylor_era5[dataset %in% c("em-earth", "mswep", "precl", "gpcp", "gpcc")]
data_for_taylor_cru <- data_for_taylor_cru[dataset %in% c("em-earth", "mswep", "precl", "gpcp", "gpcc")]

### Taylor plot
TaylorDiagram(data_for_taylor_era5, group = "dataset") 
TaylorDiagram(data_for_taylor_cru, group = "dataset") 

taylor_obs <- prec_annual[, .(obs = mean(prec_mean, na.rm = TRUE)), year]
data_for_taylor <- merge(taylor_obs, prec_annual, by = "year")
setnames(data_for_taylor, 'prec_mean', 'mod') 
TaylorDiagram(data_for_taylor, group = "dataset") 
