install.packages('kohonen')

library(kohonen)
library(corrplot)

source('source/partition_prec.R')
prec_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_datasets.rds"))

registerDoParallel(cores = N_CORES - 1)
n_pixels <- 100000 
lon_lat <- unique(prec_datasets[, .(lon, lat)])

#pixels_for_som <- lon_lat[sample(1:nrow(lon_lat), n_pixels), ]
#data_for_som <- prec_datasets[pixels_for_som, on = .(lon, lat)]

data_for_som <- copy(prec_datasets)
data_for_som <- data_for_som[sample(1:nrow(data_for_som), nrow(data_for_som)), ]
data_for_som <- dcast(data_for_som, lat + lon + year ~ dataset, value.var = 'prec')
data_for_som <- data_for_som[complete.cases(data_for_som), -c(1:3)]
data_for_som <- t(as.matrix(data_for_som))

make_freq_table <- function(dataset_cluster){
  dummy <- dcast(dataset_cluster, family~dataset)
  freq_matrix <- merge(dataset_cluster, dummy, by = 'family')[, -1]
  freq_matrix <- freq_matrix[order(dataset), ]
  freq_matrix <- as.data.frame(freq_matrix[, -1])
  rownames(freq_matrix) <- colnames(freq_matrix)
  freq_matrix[is.na(freq_matrix)] <- 0
  freq_matrix[freq_matrix >= 1] <- 1 
  return(freq_matrix)
}

dataset_clusters <- foreach(som_run = 1:1000) %dopar% {
  datasets_som <- som(data_for_som, 
                      grid = somgrid(3, 3, "hexagonal", toroidal = TRUE), 
                      rlen = 10000) 
  dataset_families <- data.table(dataset = rownames(data_for_som), family = datasets_som$unit.classif)
  make_freq_table(dataset_families)
}

dataset_clusters <- Reduce("+", dataset_clusters)/1000

corrplot(as.matrix(dataset_clusters), method = "number", type = 'upper', is.corr = FALSE) 

