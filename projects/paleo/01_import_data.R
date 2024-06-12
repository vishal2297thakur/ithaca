## Imports data from csv table and stores them as rds files.

source('source/paleo.R')

recon_summary <- read.csv(paste0(PATH_SAVE_PALEO, "/mnt/shared/data/paleo/processed/pratap_thesis/recon_meta.csv"))
recon_data_xls <- readxl::read_xls(paste0(PATH_SAVE_PALEO, "/mnt/shared/data/paleo/processed/pratap_thesis/recon_data.xls"))





















saveRDS(recon_data, paste0(PATH_SAVE_PALEO, "recon_data.rds"))