source('source/partition_prec.R')

# Partition precipitation to different regional properties and quantify their uncertainty
source('source/partition_prec.R')

## Data 
prec_annual <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_global_annual_mean.rds"))

prec_annual_sd <- prec_annual[dataset %in% PREC_GLOBAL_DATASETS, .( prec_sd = sd(prec_mean, na.rm = TRUE)), dataset]
prec_annual_sd[, range(prec_sd)]
prec_annual_sd[, median(prec_sd)]


prec_annual[dataset %in% PREC_GLOBAL_DATASETS, sd(prec_mean, na.rm = TRUE), year]
year_sd <- prec_annual[dataset %in% PREC_GLOBAL_DATASETS, sd(prec_mean, na.rm = TRUE), year]$V1
year_sd <- year_sd[complete.cases(year_sd)]
range(year_sd)
mean(year_sd)

prec_annual[dataset %in% PREC_GLOBAL_DATASETS, sd(prec_mean, na.rm = TRUE) / 
              mean(prec_mean, na.rm = TRUE), year]
prec_annual[dataset %in% PREC_GLOBAL_DATASETS, sd(prec_mean, na.rm = TRUE) / 
              mean(prec_mean, na.rm = TRUE), dataset]

prec_annual[dataset %in% PREC_GLOBAL_DATASETS, sd(prec_mean, na.rm = TRUE) / 
              mean(prec_mean, na.rm = TRUE), year]

data_for_cor <- dcast(prec_annual, year ~ dataset)
colnames(data_for_cor)[c(6, 7, 11, 15, 16)] <- c("cru", "em_earth", "gpm", "doe", "ncar")

cor_mat <- correlate(data_for_cor[, -1])

network_plot(cor_mat, min_cor = 0, legend = 'full')
