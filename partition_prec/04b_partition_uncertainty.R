source('source/partition_prec.R')

prec_annual <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_global_annual_datasets.rds"))

prec_annual[dataset %in% PREC_GLOBAL_DATASETS, sd(prec_mean, na.rm = TRUE) / 
              mean(prec_mean, na.rm = TRUE), year]
prec_annual[dataset %in% PREC_GLOBAL_DATASETS, sd(prec_mean, na.rm = TRUE) / 
              mean(prec_mean, na.rm = TRUE), dataset]

prec_annual[dataset %in% PREC_GLOBAL_DATASETS, sd(prec_mean, na.rm = TRUE) / 
              mean(prec_mean, na.rm = TRUE), year]
