# Calculate t-metric to median time series

source("source/uncertainty_prec.R")

install.packages(setdiff(c("DescTools", "Metrics", "rnaturalearth"),
                         rownames(installed.packages())))

library(Metrics)

registerDoParallel(cores = N_CORES - 1)
## Data
prec_month <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_data_month.rds"))

prec_years <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_data_years.rds"))

## Analysis
list_datasets <- unique(prec_years$dataset)

foreach (idx = 1:length(list_datasets)) %dopar% {
  data_out <- list_datasets[idx]
  dummie <- prec_month[dataset != data_out]
  dummie[, median_prec := median(prec, na.rm = TRUE), .(lon, lat, date)]
  dummie <- dummie[, .(mse_prec = mean((median_prec - prec)^2, na.rm = TRUE),
                       r_prec = cor(prec, median_prec, use = "complete.obs"),
                       bias_prec = mean(prec, na.rm = TRUE) - mean(median_prec, na.rm = TRUE),
                       var_prec = sd(prec, na.rm = TRUE)^2,
                       median_var = sd(median_prec, na.rm = TRUE)^2),
                   .(lon, lat, dataset)]
  dummie <- dummie[, .(t_prec = ((1 + r_prec)/2)*(1 - (mse_prec/(bias_prec^2 + var_prec + median_var)))),
                   .(lon, lat, dataset)]
  dummie[t_prec < 0, t_prec := 0]
  saveRDS(dummie, paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric-", data_out,
                         "_month.rds"))
}

foreach (idx = 1:length(list_datasets)) %dopar% {
  data_out <- list_datasets[idx]
  dummie <- prec_years[dataset != data_out]
  dummie[, median_prec := median(prec, na.rm = TRUE), .(lon, lat, date)]
  dummie <- dummie[, .(mse_prec = mean((median_prec - prec)^2, na.rm = TRUE),
                       r_prec = cor(prec, median_prec, use = "complete.obs"),
                       bias_prec = mean(prec, na.rm = TRUE) - mean(median_prec, na.rm = TRUE),
                       var_prec = sd(prec, na.rm = TRUE)^2,
                       median_var = sd(median_prec, na.rm = TRUE)^2),
                   .(lon, lat, dataset)]
  dummie <- dummie[, .(t_prec = ((1 + r_prec)/2)*(1 - (mse_prec/(bias_prec^2 + var_prec + median_var)))),
                   .(lon, lat, dataset)]
  dummie[t_prec < 0, t_prec := 0]
  saveRDS(dummie, paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric-", data_out,
                         "_years.rds"))
}
