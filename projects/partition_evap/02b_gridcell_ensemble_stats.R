# Estimates the ensemble statistics at each grid cell

source('source/partition_evap.R')
source('source/geo_functions.R')

## Data
evap_mean_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_datasets.rds"))

## Functions
estimate_q25 <- function(x) {as.numeric(quantile(x, 0.25, na.rm = TRUE))}
estimate_q75 <- function(x) {as.numeric(quantile(x, 0.75, na.rm = TRUE))}

## Analysis
evap_ens_stats <- evap_mean_datasets[, .(ens_mean_mean = round(mean(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]

dummy <- evap_mean_datasets[, .(dataset_count = length(unique(dataset))), .(lat, lon)]
evap_ens_stats <- merge(evap_ens_stats, dummy, by = c('lon', 'lat'))

dummy <- evap_mean_datasets[, .(ens_mean_median = round(median(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]
evap_ens_stats <- merge(evap_ens_stats, dummy, by = c('lon', 'lat'))

dummy <- evap_mean_datasets[, .(ens_mean_sd = round(sd(evap_mean, na.rm = TRUE), 2)), .(lat, lon)]
evap_ens_stats <- merge(evap_ens_stats, dummy, by = c('lon', 'lat'))

dummy <- evap_mean_datasets[, .(ens_mean_q25 = round(estimate_q25(evap_mean), 2)), .(lat, lon)]
evap_ens_stats <- merge(evap_ens_stats, dummy, by = c('lon', 'lat'))

dummy <- evap_mean_datasets[, .(ens_mean_q75 = round(estimate_q75(evap_mean), 2)), .(lat, lon)]
evap_ens_stats <- merge(evap_ens_stats, dummy, by = c('lon', 'lat'))

### Bias measures
evap_ens_stats[, std_quant_range := round((ens_mean_q75 - ens_mean_q25) / ens_mean_median, 2)] # Using q75 - q25 as in Sun et al. 2018 paper
evap_ens_stats[, ens_mean_cv := round(ens_mean_sd / ens_mean_mean, 2)]

saveRDS(evap_ens_stats, paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats.rds"))



## Code review

evap_ens_stats <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats.rds"))
library(ggplot2)

ggplot(evap_ens_stats)+
  geom_tile(aes(x = lon, y = lat, fill = ens_mean_median))+
  scale_fill_binned(type = "viridis")+
  theme_bw()


ggplot(evap_ens_stats)+
  geom_tile(aes(x = lon, y = lat, fill = dataset_count))+
  scale_fill_binned(type = "viridis", breaks = c(11.5))+
  theme_bw()

