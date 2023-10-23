# Estimation of ensemble statistics




source('source/partition_evap.R')
source('source/geo_functions_evap.R')

## Data
evap_2000_2019 <- lapply(EVAP_FNAMES_2000_2019, brick)
names(evap_2000_2019) <- EVAP_FNAMES_SHORT_2000_2019 

### Analysis
registerDoParallel(cores = N_CORES - 1)

evap_annual <- foreach(dataset_count = 1:n_datasets_2000_2019) %dopar% {
  dummie_raster <- evap_2000_2019[[dataset_count]]
  dummie_weights <- area(dummie_raster, na.rm = TRUE, weights = TRUE)
  dummie_table <- dummie_raster * dummie_weights
  dummie_table <- cellStats(dummie_table, 'sum')
  dummie_table <- data.table(getZ(dummie_raster), dummie_table)
  setnames(dummie_table, c("date", "wavg_yearly"))
  dummie_table <- dummie_table[, .(year = year(date), evap_mean =  wavg_yearly)]
}

names(evap_annual) <- names(evap_2000_2019)
#evap_annual$`gpm-imerg`[1] <- NA
evap_annual <- bind_rows(evap_annual, .id = "column_label")
colnames(evap_annual)[1] <- "dataset"
evap_annual[, evap_volume := GLOBAL_AREA * M2_TO_KM2 * evap_mean * MM_TO_KM]

saveRDS(evap_annual, paste0(PATH_SAVE_PARTITION_EVAP, "evap_global_annual_mean.rds"))



