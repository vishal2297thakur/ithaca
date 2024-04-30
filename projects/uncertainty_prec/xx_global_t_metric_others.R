# Calculate RMSE to median time series excluding the worst per grid cell
source("source/uncertainty_prec.R")

install.packages(setdiff(c("Metrics", "rnaturalearth"),
                         rownames(installed.packages())))

library(Metrics)
library(rnaturalearth)
library(sf)
library(stars)

## Data
prec_month <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_data_month.rds"))

t_min <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_month.rds"))
t_min <- t_min[, .SD[which.min(t_prec)], by = .(lon, lat)]
t_min <- t_min[, .(lon, lat, dataset)]

top_10 <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_month.rds"))
top_10 <- top_10[complete.cases(top_10)]
setorder(top_10, -t_prec)
top_10 <- top_10[, .SD[1:10], .(lon, lat)]
top_10 <- top_10[, .(lon, lat, dataset)]

## Analysis
prec_no_min <- prec_month[!t_min, on = .(lon, lat, dataset)]

prec_top10 <- prec_month[top_10, on = .(lon, lat, dataset)]

prec_no_ncep <- prec_month[dataset != "ncep-doe" & dataset != "ncep-ncar"]

prec_no_gpm_mswep <- prec_month[dataset != "gpm-imerg" & dataset != "mswep" & dataset != "gsmap"]

prec_no_min[, median_prec := median(prec, na.rm = TRUE), .(lon, lat, date)]
prec_no_min <- prec_no_min[, .(mse_prec = mean((median_prec - prec)^2,
                                               na.rm = TRUE),
                               r_prec = cor(prec, median_prec,
                                            use = "complete.obs"),
                               bias_prec = mean(prec, na.rm = TRUE) -
                                 mean(median_prec, na.rm = TRUE),
                               var_prec = sd(prec, na.rm = TRUE)^2,
                               median_var = sd(median_prec, na.rm = TRUE)^2),
                           .(lon, lat, dataset)]

prec_no_min <- prec_no_min[, .(t_prec = ((1 + r_prec)/2)*(1 - (mse_prec/(bias_prec^2 + var_prec + median_var)))),
                           .(lon, lat, dataset)]

prec_no_min[t_prec < 0, t_prec := 0]

prec_top10[, median_prec := median(prec, na.rm = TRUE), .(lon, lat, date)]
prec_top10 <- prec_top10[, .(mse_prec = mean((median_prec - prec)^2,
                                             na.rm = TRUE),
                             r_prec = cor(prec, median_prec,
                                          use = "complete.obs"),
                             bias_prec = mean(prec, na.rm = TRUE) -
                               mean(median_prec, na.rm = TRUE),
                             var_prec = sd(prec, na.rm = TRUE)^2,
                             median_var = sd(median_prec, na.rm = TRUE)^2),
                         .(lon, lat, dataset)]

prec_top10 <- prec_top10[, .(t_prec = ((1 + r_prec)/2)*(1 - (mse_prec/(bias_prec^2 + var_prec + median_var)))),
                         .(lon, lat, dataset)]

prec_top10[t_prec < 0, t_prec := 0]

prec_no_ncep[, median_prec := median(prec, na.rm = TRUE), .(lon, lat, date)]
prec_no_ncep <- prec_no_ncep[, .(mse_prec = mean((median_prec - prec)^2,
                                                 na.rm = TRUE),
                                 r_prec = cor(prec, median_prec,
                                              use = "complete.obs"),
                                 bias_prec = mean(prec, na.rm = TRUE) -
                                   mean(median_prec, na.rm = TRUE),
                                 var_prec = sd(prec, na.rm = TRUE)^2,
                                 median_var = sd(median_prec, na.rm = TRUE)^2),
                             .(lon, lat, dataset)]

prec_no_ncep <- prec_no_ncep[, .(t_prec = ((1 + r_prec)/2)*(1 - (mse_prec/(bias_prec^2 + var_prec + median_var)))),
                             .(lon, lat, dataset)]

prec_no_ncep[t_prec < 0, t_prec := 0]

prec_no_gpm_mswep[, median_prec := median(prec, na.rm = TRUE), .(lon, lat, date)]
prec_no_gpm_mswep <- prec_no_gpm_mswep[, .(mse_prec = mean((median_prec - prec)^2,
                                                           na.rm = TRUE),
                                           r_prec = cor(prec, median_prec,
                                                        use = "complete.obs"),
                                           bias_prec = mean(prec, na.rm = TRUE) -
                                             mean(median_prec, na.rm = TRUE),
                                           var_prec = sd(prec, na.rm = TRUE)^2,
                                           median_var = sd(median_prec, na.rm = TRUE)^2),
                                       .(lon, lat, dataset)]

prec_no_gpm_mswep <- prec_no_gpm_mswep[, .(t_prec = ((1 + r_prec)/2)*(1 - (mse_prec/(bias_prec^2 + var_prec + median_var)))),
                                       .(lon, lat, dataset)]

prec_no_gpm_mswep[t_prec < 0, t_prec := 0]

## Save
saveRDS(prec_no_min, paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_min.rds"))
saveRDS(prec_top10, paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_top10.rds"))
saveRDS(prec_no_ncep, paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_ncep.rds"))
saveRDS(prec_no_gpm_mswep, paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_gpm.rds"))
