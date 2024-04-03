# Transform data sets from brick to a single data table (large memory requirements)
source("source/uncertainty_prec.R")

## Data
load(paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_names_2001_2019.rda"))

registerDoParallel(cores = 24)

## Analysis
prec_data <- foreach(data_count = 1:length(PREC_NAMES_2001_2019), .combine = rbind) %do% {
  dummie_brick <- brick(PREC_NAMES_2001_2019[data_count])
  n_layers <- nlayers(dummie_brick)
  dummie_table <- foreach(idx = 1:n_layers, .combine = rbind) %dopar% {
   dummie_layer <- as.data.frame(dummie_brick[[idx]], long = TRUE, xy = TRUE,
                                 na.rm = TRUE) %>% as.data.table()
   dummie_name <- filename(dummie_brick)
   dummie_name <- sub(".*/([^_/]*)_.*", "\\1", dummie_name)
   dummie_layer$dataset <- dummie_name
   return(dummie_layer)
  }
  dummie_table <- dummie_table[, .(lon = x, lat = y, date = Z, dataset,
                                  prec = value)]
  return(dummie_table)
}

## Save data
saveRDS(prec_data, paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_data.rds"))
