### Estimation of ensemble statistics
source('source/blueprint.R')
source('source/geo_functions.R')
source('source/graphics.R')

## Read data 
prec_2000_2019 <- lapply(prec_fnames_2000_2019_kenya, brick)
names(prec_2000_2019) <- prec_fnames_short_2000_2019_kenya 

## Main estimations
prec_2000_2009 <- lapply(prec_2000_2019, subset, 1:115)
prec_2010_2019 <- lapply(prec_2000_2019, subset, 116:235)

prec_mean_2000_2009 <- lapply(prec_2000_2009, calc, fun = mean)
prec_mean_2010_2019 <- lapply(prec_2010_2019, calc, fun = mean)

prec_mean_2000_2009_dt <- foreach(dataset_count = 1:n_datasets_2000_2019, .combine = rbind) %dopar% {
  dummy <- data.table(as.data.frame(rasterToPoints(prec_mean_2000_2009[[dataset_count]], spatial = TRUE)))
  colnames(dummy) <- c('prec_mean_00_09', 'lon', 'lat')
  dummy$dataset <- names(prec_mean_2000_2009[dataset_count])
  dummy[, lon := round(lon, 3)]
  dummy[, lat := round(lat, 3)]
  dummy
}

prec_mean_2010_2019_dt <- foreach(dataset_count = 1:n_datasets_2000_2019, .combine = rbind) %dopar% {
  dummy <- data.table(as.data.frame(rasterToPoints(prec_mean_2010_2019[[dataset_count]], spatial = TRUE)))
  colnames(dummy) <- c('prec_mean_10_19', 'lon', 'lat')
  dummy$dataset <- names(prec_mean_2010_2019[dataset_count])
  dummy[, lon := round(lon, 3)]
  dummy[, lat := round(lat, 3)]
  dummy
}

prec_change <- merge(prec_mean_2000_2009_dt, prec_mean_2010_2019_dt, by = c("lat", "lon", "dataset"))
prec_change[, prec_mean_00_09 := round(prec_mean_00_09, 0)]
prec_change[, prec_mean_10_19 := round(prec_mean_10_19, 0)]
prec_change[, abs_change := (prec_mean_10_19 - prec_mean_00_09)]
prec_change[, rel_change := round(abs_change / prec_mean_00_09, 2)]

prec_change[, n_datasets := .N, .(lon, lat)]
prec_change[, n_datasets_pos := 0, .(lon, lat)]
prec_change[, n_datasets_neg := 0, .(lon, lat)]
prec_change[abs_change > 0, n_datasets_pos := .N, .(lon, lat)]
prec_change[abs_change < 0, n_datasets_neg := .N, .(lon, lat)]
prec_change[, change_agreement := FALSE]
prec_change[n_datasets_pos >= 9 | n_datasets_neg >= 9, change_agreement := TRUE, .(lon, lat)]

prec_change[, table(n_datasets)]
prec_change[, table(n_datasets_pos)]
prec_change[, table(n_datasets_neg)]
sum(prec_change[, table(n_datasets_neg)])
sum(prec_change[, table(n_datasets_pos)])
sum(prec_change[, table(n_datasets)])
prec_change[, table(change_agreement)]

prec_change[change_agreement == TRUE, table(dataset)]

