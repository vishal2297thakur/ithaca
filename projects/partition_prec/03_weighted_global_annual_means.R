# Estimates the mean global precipiation (mm and volume) per dataset and year
# Weighted mean was used to account for different grid cell size
# Only datasets with global coverage are used

source('source/partition_prec.R')
source('source/geo_functions.R')

## Data
prec_2000_2019 <- lapply(PREC_FNAMES_2000_2019, brick)
names(prec_2000_2019) <- PREC_FNAMES_SHORT_2000_2019 
names_prec_2000_2019_global <- PREC_GLOBAL_DATASETS[PREC_GLOBAL_DATASETS %in% PREC_FNAMES_SHORT_2000_2019]
prec_2000_2019_global <- prec_2000_2019[names_prec_2000_2019_global]
n_datasets_2000_2019_global <- length(prec_2000_2019_global)

### Analysis
registerDoParallel(cores = N_CORES - 1)

prec_annual <- foreach(dataset_count = 1:n_datasets_2000_2019_global) %dopar% {
  dummie_raster <- names_prec_2000_2019_global[[dataset_count]]
  dummie_weights <- area(dummie_raster, na.rm = TRUE, weights = TRUE)
  dummie_table <- dummie_raster * dummie_weights
  dummie_table <- cellStats(dummie_table, 'sum')
  dummie_table <- data.table(getZ(dummie_raster), dummie_table)
  setnames(dummie_table, c("date", "wavg_yearly"))
  dummie_table <- dummie_table[, .(year = year(date), prec_mean =  wavg_yearly)]
}
names(prec_annual) <- names(names_prec_2000_2019_global)
prec_annual$`gpm-imerg`[1] <- NA
prec_annual <- bind_rows(prec_annual, .id = "column_label")
colnames(prec_annual)[1] <- "dataset"
prec_annual[, prec_volume := GLOBAL_AREA * M2_TO_KM2 * prec_mean * MM_TO_KM]

saveRDS(prec_annual, paste0(PATH_SAVE_PARTITION_PREC, "prec_global_annual_mean.rds"))
