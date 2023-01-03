source('source/masks_source.R')
source('source/geo_functions.R')
source('source/example_kenya.R')
source('source/plot_functions')

### Estimation of monthly precipitation mean, standard deviation (sd), and coefficient of variance
### for the dataset ensemble

# Read data 
prec_era5_kenya <- readRDS(paste0(path_save_kenya, "prec_era5.rds"))
prec_terra_kenya <- readRDS(paste0(path_save_kenya, "prec_terra.rds"))

# Variables
period_months_dates <- seq(PERIOD_START, by = "month", length.out = period_months)

## Monthly ensemble mean for two data sources 
# Version 1: Parallel computing
prec_mean_month <- foreach(month_count = 1:period_months) %dopar% {
   calc(stack(prec_era5_kenya[[month_count]], 
             prec_terra_kenya[[month_count]]), 
       fun = mean, 
       na.rm = F)
}

prec_mean_month <- brick(prec_mean_month)
prec_mean_month <- setZ(prec_mean_month, period_months_dates)
names(prec_mean_month) <- as.Date(period_months_dates)

prec_sd_month <- foreach(month_count = 1:period_months) %dopar% {
  calc(stack(prec_era5_kenya[[month_count]], 
             prec_terra_kenya[[month_count]]), 
       fun = sd, 
       na.rm = F)
}

prec_sd_month <- brick(prec_sd_month)
prec_sd_month <- setZ(prec_sd_month, period_months_dates)
names(prec_sd_month) <- as.Date(period_months_dates)

prec_cv_month <- foreach(month_count = 1:period_months) %dopar% {
  prec_sd_month[[month_count]]/prec_mean_month[[month_count]]
}

prec_cv_month <- brick(prec_cv_month)
prec_cv_month <- setZ(prec_cv_month, period_months_dates)
names(prec_cv_month) <- as.Date(period_months_dates)

# Quick validation
plot(mean(prec_era5_kenya))
plot(mean(prec_terra_kenya))
plot(mean(prec_mean_month))
plot(mean(prec_sd_month))
plot(mean(prec_cv_month))

# Transforming to data.table 
prec_mean_month_dt <- brick_to_dt(prec_mean_month) #function stored in geofunctions.R
prec_sd_month_dt <- brick_to_dt(prec_sd_month)
prec_cv_month_dt <- brick_to_dt(prec_cv_month)


## Version 2: Alternative example with stackApply 
## Not sure which of two versions is faster with big rasters

prec_mean_month_alt <- stackApply(x = stack(prec_era5_kenya, prec_terra_kenya), 
                           indices = rep(1:period_months, n_datasets), 
                           fun = mean, 
                           na.rm = F)
prec_mean_month_alt <- setZ(prec_mean_month_alt, period_months_dates)
names(prec_mean_month_alt) <- as.Date(period_months_dates)

prec_mean_month_alt_dt <- brick_to_dt(prec_mean_month_alt)

## Plotting

ggmap(prec_mean_month_dt[time == period_months_dates[[120]]])

ggmap(prec_sd_month_dt[time == period_months_dates[[120]]])

ggmap(prec_cv_month_dt[time == period_months_dates[[120]]])
