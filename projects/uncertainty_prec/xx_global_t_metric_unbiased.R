# Calculate RMSE to median time series
source("source/uncertainty_prec.R")

install.packages(setdiff("Metrics", rownames(installed.packages())))

library(Metrics)

## Data
prec_month <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_data_month.rds"))

## Analysis
prec_median <- prec_month[dataset != "gpm-imerg" & dataset != "mswep" &
                            dataset != "gsmap",
                          .(median_prec = median(prec, na.rm = TRUE)),
                          .(lon, lat, date)]

prec_month <- merge(prec_month, prec_median, by = c("lon", "lat", "date"))
prec_month <- prec_month[, .(mse_prec = mean((median_prec - prec)^2,
                                             na.rm = TRUE),
                             r_prec = cor(prec, median_prec,
                                          use = "complete.obs"),
                             bias_prec = mean(prec, na.rm = TRUE) -
                               mean(median_prec, na.rm = TRUE),
                             var_prec = sd(prec, na.rm = TRUE)^2,
                             median_var = sd(median_prec, na.rm = TRUE)^2),
                         .(lon, lat, dataset)]

prec_month <- prec_month[, .(t_prec = ((1 + r_prec)/2)*(1 - (mse_prec/(bias_prec^2 + var_prec + median_var)))),
                         .(lon, lat, dataset)]

prec_month[t_prec < 0, t_prec := 0]


## Save
saveRDS(prec_month, paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_unbiased.rds"))
