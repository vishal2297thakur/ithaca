# Calculate T-metric
source("source/uncertainty_prec.R")

## Data
prec_data <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_data_roi.rds"))

PREC_REPS <- c("cmap", "cpc", "cru-ts-v4-07", "gpcp-v3-2", "em-earth",
               "era5-land", "ncep-doe")

## Analysis
prec_median <- prec_data[dataset %in% PREC_REPS]
prec_median <- prec_median[, .(median_prec = median(prec, na.rm = TRUE)), .(lon,
                               lat, date)]

prec_data <- merge(prec_median, prec_data, by = c("lon", "lat", "date"),
                   allow.cartesian = TRUE)

prec_data <- prec_data[, .(mse_prec = mean((median_prec - prec)^2, na.rm = TRUE),
                             r_prec = cor(prec, median_prec, use = "pairwise.complete.obs"),
                             bias_prec = mean(prec, na.rm = TRUE) - mean(median_prec, na.rm = TRUE),
                             var_prec = sd(prec, na.rm = TRUE)^2,
                             median_var = sd(median_prec, na.rm = TRUE)^2),
                         .(lon, lat, dataset)]

prec_data <- prec_data[, .(t_prec = ((1 + r_prec)/2)*(1 - (mse_prec/(bias_prec^2 + var_prec + median_var)))),
                         .(lon, lat, dataset)]

prec_data[t_prec < 0, t_prec := 0]

## Save
saveRDS(prec_data, paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric.rds"))
