# Calculate trend agreement ----

source('source/evap_trend.R')
source('source/geo_functions.R')
source('source/graphics.R')

## Data ----
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_evap_area.rds"))  

## Analysis ----
### postiive trend ----
evap_ens_stats_pos <- evap_trend[theil_sen_slope >= 0, .(ens_trend_mean = round(mean(theil_sen_slope, na.rm = TRUE), 2)), .(lat, lon)]

dummy <- evap_trend[theil_sen_slope >= 0,  .(dataset_count = .N), .(lat, lon)]
evap_ens_stats_pos <- merge(evap_ens_stats_pos, dummy, by = c('lon', 'lat'))

dummy <- evap_trend[theil_sen_slope >= 0, .(ens_trend_median = round(median(theil_sen_slope, na.rm = T), 2)), .(lat, lon)]
evap_ens_stats_pos <- merge(evap_ens_stats_pos, dummy, by = c('lon', 'lat'))

dummy <- evap_trend[theil_sen_slope >= 0, .(ens_trend_sd = round(sd(theil_sen_slope, na.rm = T), 2)), .(lat, lon)]
evap_ens_stats_pos <- merge(evap_ens_stats_pos, dummy, by = c('lon', 'lat'))

dummy <- evap_trend[theil_sen_slope >= 0, .(ens_trend_q25 = round(quantile(theil_sen_slope, 0.25, na.rm = T), 2)), .(lat, lon)]
evap_ens_stats_pos <- merge(evap_ens_stats_pos, dummy, by = c('lon', 'lat'))

dummy <- evap_trend[theil_sen_slope >= 0, .(ens_trend_q75 = round(quantile(theil_sen_slope, 0.75, na.rm = T), 2)), .(lat, lon)]
evap_ens_stats_pos <- merge(evap_ens_stats_pos, dummy, by = c('lon', 'lat'))

### negative trend ----
evap_ens_stats_neg <- evap_trend[theil_sen_slope < 0, .(ens_trend_mean = round(mean(theil_sen_slope, na.rm = TRUE), 2)), .(lat, lon)]

dummy <- evap_trend[theil_sen_slope < 0,  .(dataset_count = .N), .(lat, lon)]
evap_ens_stats_neg <- merge(evap_ens_stats_neg, dummy, by = c('lon', 'lat'))

dummy <- evap_trend[theil_sen_slope < 0, .(ens_trend_median = round(median(theil_sen_slope, na.rm = T), 2)), .(lat, lon)]
evap_ens_stats_neg <- merge(evap_ens_stats_neg, dummy, by = c('lon', 'lat'))

dummy <- evap_trend[theil_sen_slope < 0, .(ens_trend_sd = round(sd(theil_sen_slope, na.rm = T), 2)), .(lat, lon)]
evap_ens_stats_neg <- merge(evap_ens_stats_neg, dummy, by = c('lon', 'lat'))

dummy <- evap_trend[theil_sen_slope < 0, .(ens_trend_q25 = round(quantile(theil_sen_slope, 0.25, na.rm = T), 2)), .(lat, lon)]
evap_ens_stats_neg <- merge(evap_ens_stats_neg, dummy, by = c('lon', 'lat'))

dummy <- evap_trend[theil_sen_slope < 0, .(ens_trend_q75 = round(quantile(theil_sen_slope, 0.75, na.rm = T), 2)), .(lat, lon)]
evap_ens_stats_neg <- merge(evap_ens_stats_neg, dummy, by = c('lon', 'lat'))

#### Bias measures ----
evap_ens_stats_pos[, std_quant_range := round((ens_trend_q75 - ens_trend_q25) / ens_trend_median, 2)] 
evap_ens_stats_pos[, ens_trend_cv := round(ens_trend_sd / ens_trend_mean, 2)] 

evap_ens_stats_neg[, std_quant_range := round((ens_trend_q75 - ens_trend_q25) / ens_trend_median, 2)] 
evap_ens_stats_neg[, ens_trend_cv := round(ens_trend_sd / ens_trend_mean, 2)] 

### merge and save ----
evap_ens_stats <- merge(evap_ens_stats_pos, evap_ens_stats_neg, by = c('lat', 'lon'), all = TRUE)
evap_ens_stats[is.na(dataset_count.x), dataset_count.x := 0] 
evap_ens_stats[is.na(dataset_count.y), dataset_count.y := 0] 

evap_ens_stats[,dataset_count_total := dataset_count.x+dataset_count.y]
evap_ens_stats[dataset_count_total < n_datasets_2000_2019, flag := TRUE]

saveRDS(evap_ens_stats, paste0(PATH_SAVE_EVAP_TREND, "evap_trend_ensemble_stats_pos_neg.rds"))

# check ----

data_to_plot <- evap_ens_stats[, .(dataset_count.x, dataset_count.y, flag)]
data_to_plot <- melt(data_to_plot, id.vars = "flag")
data_to_plot[variable == "dataset_count.x", variable := "positive slope"]
data_to_plot[variable == "dataset_count.y", variable := "negative slope"]


ggplot(data_to_plot[is.na(flag)])+
  geom_bar(aes(y = value, col = as.factor(variable), fill = as.factor(variable)), position = "dodge")+
  scale_color_manual(values = c("red", "royalblue2"))+
  scale_fill_manual(values = c("red", "royalblue2"))+
  labs(fill = "slope direction", col = "slope direction", y = "dataset count per grid")+
  theme_bw()

ggplot(data_to_plot[is.na(flag)])+
  geom_bar(aes(y = value, col = variable, fill = as.factor(variable)), position = "stack")+
  scale_color_manual(values = c("red", "royalblue2"))+
  scale_fill_manual(values = c("red", "royalblue2"))+
  labs(fill = "slope direction", col = "slope direction", y = "dataset count per grid")+
  annotate("text", y = 13, x = 25000, label= "grids where all datasets have the same slope direction")+
  lims(y = c(1,14))+
  theme_bw()
