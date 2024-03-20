# Trend agreement ----
source('source/evap_trend.R')
source('source/geo_functions.R')
source('source/graphics.R')

## Data
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_evap_area.rds"))  

## Analysis
evap_ens_stats <- evap_trend[, .(ens_trend_mean = round(mean(theil_sen_slope, na.rm = TRUE), 2)), .(lat, lon)]

dummy <- evap_trend[,  .(dataset_count = .N), .(lat, lon)]
evap_ens_stats <- merge(evap_ens_stats, dummy, by = c('lon', 'lat'))

dummy <- evap_trend[, .(ens_trend_median = round(median(theil_sen_slope, na.rm = T), 2)), .(lat, lon)]
evap_ens_stats <- merge(evap_ens_stats, dummy, by = c('lon', 'lat'))

dummy <- evap_trend[, .(ens_trend_sd = round(sd(theil_sen_slope, na.rm = T), 2)), .(lat, lon)]
evap_ens_stats <- merge(evap_ens_stats, dummy, by = c('lon', 'lat'))

dummy <- evap_trend[, .(ens_trend_q25 = round(quantile(theil_sen_slope, 0.25, na.rm = T), 2)), .(lat, lon)]
evap_ens_stats <- merge(evap_ens_stats, dummy, by = c('lon', 'lat'))

dummy <- evap_trend[, .(ens_trend_q75 = round(quantile(theil_sen_slope, 0.75, na.rm = T), 2)), .(lat, lon)]
evap_ens_stats <- merge(evap_ens_stats, dummy, by = c('lon', 'lat'))


evap_ens_stats[ens_trend_q25 < 0 & ens_trend_q75 > 0, flag := TRUE]
evap_ens_stats[ens_trend_q25 > 0 & ens_trend_q75 > 0, flag := FALSE]
evap_ens_stats[ens_trend_q25 < 0 & ens_trend_q75 < 0, flag := FALSE]

evap_ens_stats[, std_quant_range := round((ens_trend_q75 - ens_trend_q25) / ens_trend_median, 2)] 

evap_ens_stats[, ens_trend_cv := round(ens_trend_sd / ens_trend_mean, 2)] 

evap_ens_stats[, abs_std_quant_range := abs(std_quant_range)] 


## save ----
saveRDS(evap_ens_stats, paste0(PATH_SAVE_EVAP_TREND, "evap_trend_ensemble_stats_all.rds"))

## check ----

evap_ens_stats[flag == TRUE, .N]

ggplot(evap_ens_stats[flag == "FALSE",])+
  geom_histogram(aes(x = std_quant_range))+
  theme_bw()

ggplot(evap_ens_stats[flag == "FALSE",])+
  geom_histogram(aes(x = abs_std_quant_range))+
  theme_bw()

ggplot(evap_ens_stats)+
  geom_histogram(aes(x = abs_std_quant_range))+
  theme_bw()

ggplot(evap_ens_stats)+
  geom_histogram(aes(x = std_quant_range))+
  theme_bw()
