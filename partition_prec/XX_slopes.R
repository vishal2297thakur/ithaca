source('source/partition_prec.R')
source('source/geo_functions.R')
source('source/graphics.R')

prec_2000_2019 <- lapply(PREC_FNAMES_2000_2019, brick)
names(prec_2000_2019) <- PREC_FNAMES_SHORT_2000_2019

registerDoParallel(cores = N_CORES - 1)
prec_slopes <- foreach(dataset_count = 1:n_datasets_2000_2019) %dopar% {
  dummie <- prec_2000_2019[[dataset_count]]
  dummie_slopes <- brick_slopes(dummie, annual = 'sum')
  dummie_table <- data.table(as.data.frame(dummie_slopes, xy = TRUE, na.rm = TRUE))
  dummie_table <- dummie_table[layer != 0]
  dummie_table[, .(lon = x, lat = y, slope = layer,
                   dataset = names(prec_2000_2019)[dataset_count])]
}
prec_slopes <- rbindlist(prec_slopes)


dataset_count <- 1
dummy <- brick_slopes(prec_2000_2019[[dataset_count]], annual = 'sum')
dummy_dt <- data.table(as.data.frame(dummy, xy = TRUE))
dummy_dt <- dummy_dt[complete.cases(dummy_dt)]


prec_slopes <-  readRDS(paste0(PATH_SAVE_PARTITION_PREC, "test_slopes.rds"))

makeCluster(N_CORES - 2, type = "PSOCK")
cluster <- makeCluster(N_CORES - 2, type = "PSOCK")
dataset_count <- 3
dummy <- brick_slopes(prec_2000_2019[[dataset_count]], annual = 'sum')
dummy_dt <- data.table(as.data.frame(dummy, xy = TRUE))
dummy_dt <- dummy_dt[complete.cases(dummy_dt)]

test <- rbind(prec_slopes, 
                     dummy_dt[, .(lon = x, lat = y, slope, dataset = names(prec_2000_2019)[dataset_count])])

dummy_dt[slope < 0, slope_sign := factor('neg')]
dummy_dt[slope > 0, slope_sign := factor('pos')]
ggplot(dummy_dt[slope > 5 | slope < -5]) +
  geom_point(aes(x, y, col = slope_sign))

saveRDS(prec_slopes, paste0(PATH_SAVE_PARTITION_PREC, "test_slopes.rds"))




