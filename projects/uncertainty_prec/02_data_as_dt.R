# Transform data sets from brick to a single data table (large memory requirements)

source('source/uncertainty_prec.R')

load(paste0(PATH_SAVE_UNCERTAINTY_PREC, 'prec_names_2000_2019.rda'))

## Data 
prec_2000_2019 <- lapply(PREC_NAMES_2000_2019, brick)
names(prec_2000_2019) <- PREC_NAMES_SHORT_2000_2019
n_datasets_2000_2019 <- length(prec_2000_2019)

## Analysis
registerDoParallel(cores = N_CORES - 1)

prec_datasets <- foreach(data_count = 1:n_datasets_2000_2019, .combine = rbind) %do% {
  dummie_brick <- prec_2000_2019[[data_count]]
  n_layers <- nlayers(dummie_brick)
  dummie_table <- foreach(idx = 1:n_layers, .combine = rbind) %dopar% {
    dummie_layer <- as.data.frame(dummie_brick[[idx]], long = TRUE, xy = TRUE,
                                  na.rm = TRUE) %>% as.data.table()
    dummie_name <- filename(dummie_brick)
    dummie_name <- sub('.*/([^_/]*)_.*', '\\1', dummie_name)
    dummie_layer$dataset <- dummie_name
    return(dummie_layer)
  }
  dummie_table <- dummie_table[, .(lon = x, lat = y, date = Z, dataset,
                                   prec = value)]
  return(dummie_table)
}

## Save data
saveRDS(prec_datasets, paste0(PATH_SAVE_UNCERTAINTY_PREC, 'prec_datasets.rds'))
