### Estimation of ensemble statistics
install.packages("gtools")
library("gtools")

source('source/blueprint.R')
source('source/geo_functions.R')
source('source/graphics.R')

## Read data 
prec_2000_2019 <- lapply(prec_fnames_2000_2019_kenya, brick)
names(prec_2000_2019) <- prec_fnames_short_2000_2019_kenya 

## Set functions
estimate_q25 <- function(x) {as.numeric(quantile(x, 0.25, na.rm = TRUE))}
estimate_q75 <- function(x) {as.numeric(quantile(x, 0.75, na.rm = TRUE))}

prec_ens_mean_month <- foreach(month_count = 1:period_months, .packages = 'raster') %dopar% {
  pick_rasters <- sapply(prec_2000_2019, "[[", month_count)
  calc(stack(pick_rasters), fun = mean, na.rm = T)
}
prec_ens_mean_month <- stack(prec_ens_mean_month)
prec_ens_mean_month <- setZ(prec_ens_mean_month, period_months_dates)
names(prec_ens_mean_month) <- as.Date(period_months_dates)
prec_ens_mean_month_dt <- brick_to_dt(prec_ens_mean_month) 
colnames(prec_ens_mean_month_dt) <-  c('lon', 'lat', 'time', 'ens_mean')

prec_ens_mean_sd_month <- foreach(month_count = 1:period_months, .packages = 'raster') %dopar% {
  pick_rasters <- sapply(prec_2000_2019, "[[", month_count)
  calc(stack(pick_rasters), fun = sd, na.rm = T)
}
prec_ens_mean_sd_month <- stack(prec_ens_mean_sd_month)
prec_ens_mean_sd_month <- setZ(prec_ens_mean_sd_month, period_months_dates)
names(prec_ens_mean_sd_month) <- as.Date(period_months_dates)
prec_ens_mean_sd_month_dt <- brick_to_dt(prec_ens_mean_sd_month) 
colnames(prec_ens_mean_sd_month_dt) <-  c('lon', 'lat', 'time', 'ens_mean_sd')

prec_ens_mean_q25_month <- foreach(month_count = 1:period_months, .packages = 'raster') %dopar% {
  pick_rasters <- sapply(prec_2000_2019, "[[", month_count)
  calc(stack(pick_rasters), fun = estimate_q25)
}
prec_ens_mean_q25_month <- stack(prec_ens_mean_q25_month)
prec_ens_mean_q25_month <- setZ(prec_ens_mean_q25_month, period_months_dates)
names(prec_ens_mean_q25_month) <- as.Date(period_months_dates)
prec_ens_mean_q25_month_dt <- brick_to_dt(prec_ens_mean_q25_month) 
colnames(prec_ens_mean_q25_month_dt) <-  c('lon', 'lat', 'time', 'ens_mean_q25')

prec_ens_mean_q75_month <- foreach(month_count = 1:period_months, .packages = 'raster') %dopar% {
  pick_rasters <- sapply(prec_2000_2019, "[[", month_count)
  calc(stack(pick_rasters), fun = estimate_q75)
}
prec_ens_mean_q75_month <- stack(prec_ens_mean_q75_month)
prec_ens_mean_q75_month <- setZ(prec_ens_mean_q75_month, period_months_dates)
names(prec_ens_mean_q75_month) <- as.Date(period_months_dates)
prec_ens_mean_q75_month_dt <- brick_to_dt(prec_ens_mean_q75_month) 
colnames(prec_ens_mean_q75_month_dt) <-  c('lon', 'lat', 'time', 'ens_mean_q75')

prec_ens_stats_month <- merge(prec_ens_mean_month_dt, prec_ens_mean_sd_month_dt, by = c('lon', 'lat', 'time'))
prec_ens_stats_month <- merge(prec_ens_stats_month, prec_ens_mean_q25_month_dt, by = c('lon', 'lat', 'time'))
prec_ens_stats_month <- merge(prec_ens_stats_month, prec_ens_mean_q75_month_dt, by = c('lon', 'lat', 'time'))

prec_ens_stats_month[, month := month(time)]
prec_ens_stats_month[, year := year(time)]
prec_ens_stats_month <- prec_ens_stats_month[, .(lon, lat, time, month, year, ens_mean, ens_mean_sd, ens_mean_q25, ens_mean_q75)]
prec_ens_stats_month[, std_quant_range := (ens_mean_q75 - ens_mean_q25) / ens_mean] # Using q75 - q25 as in Sun et al. 2018 paper
prec_ens_stats_month[, ens_mean_cv := ens_mean_sd / ens_mean]
prec_ens_stats_month[, quant_ens_cv := ordered(quantcut(ens_mean_cv, 5), 
                                               labels = c('0-0.2', '0.2-0.4', '0.4-0.6', '0.6-0.8', '0.8-1.00'))]
prec_ens_stats_month[, dataset_agreement := ordered(quantcut(std_quant_range, 5), 
                                                    labels = c('high', 'above average', 'average', 'below average', 'low'))]


prec_ens_stats_month_mean <- prec_ens_stats_month[, lapply(.SD, mean), 
                                                  .SDcols =  c('ens_mean', 'ens_mean_sd', 'ens_mean_cv', 'ens_mean_q25', 'ens_mean_q75', 'std_quant_range'), 
                                                  by = c('lon', 'lat', 'month')]


saveRDS(prec_ens_stats_month, paste0(path_save_blueprint, "prec_ensemble_stats_month.rds"))
saveRDS(prec_ens_stats_month_mean, paste0(path_save_blueprint, "prec_ensemble_stats_month_mean.rds"))