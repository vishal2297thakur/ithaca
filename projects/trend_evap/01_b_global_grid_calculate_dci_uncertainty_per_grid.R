# Calculate dataset concurrence index for each grid ----
# Calculate uncertainty for each grid ----
# Calculate count of grid directions for each grid ----

source('source/evap_trend.R')
source('source/geo_functions.R')
source('source/graphics.R')

## Data ----
### Input data generated in 01_a
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope.rds"))  

evap_trend <- evap_trend[complete.cases(evap_trend)]


## Analysis 
### DCI ----
evap_trend_dci <- evap_trend[,.(DCI_theil_sen = sum(theil_sen_slope/abs(theil_sen_slope)*significant_theil_sen, na.rm = TRUE)/.N),
.(lon,lat)]


### probability groups ----
evap_trend_pos <- evap_trend[trend_direction == "positive significant", .(N_pos_theil_sen = .N), .(lon,lat)]
evap_trend_neg <- evap_trend[trend_direction == "negative significant", .(N_neg_theil_sen = .N), .(lon,lat)]
evap_trend_ins_pos <- evap_trend[trend_direction == "positive", .(N_ins_pos_theil_sen = .N), .(lon,lat)]
evap_trend_ins_neg <- evap_trend[trend_direction == "negative", .(N_ins_neg_theil_sen = .N), .(lon,lat)]

evap_trend_summary <- merge(evap_trend_pos, evap_trend_ins_pos, by = c("lon", "lat"), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary , evap_trend_neg, by = c("lon", "lat"), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary , evap_trend_ins_neg, by = c("lon", "lat"), all = TRUE)

evap_trend_summary[is.na(N_pos_theil_sen), N_pos_theil_sen:= 0]
evap_trend_summary[is.na(N_neg_theil_sen), N_neg_theil_sen:= 0]
evap_trend_summary[is.na(N_ins_pos_theil_sen), N_ins_pos_theil_sen:= 0]
evap_trend_summary[is.na(N_ins_neg_theil_sen), N_ins_neg_theil_sen:= 0]

evap_trend_summary[, count:= sum(N_pos_theil_sen, N_neg_theil_sen, N_ins_pos_theil_sen, N_ins_neg_theil_sen, na.rm = T),.(lon,lat)]
evap_trend_summary[, trend:= "no trend",]
evap_trend_summary[N_pos_theil_sen > 5 & N_neg_theil_sen == 0, trend:= "positive likely",]
evap_trend_summary[N_pos_theil_sen > 1 & N_pos_theil_sen <= 5 & N_neg_theil_sen == 0, trend:= "positive probable",]

evap_trend_summary[N_neg_theil_sen > 5  & N_pos_theil_sen == 0, trend:= "negative likely",]
evap_trend_summary[N_neg_theil_sen > 1 & N_neg_theil_sen <= 5 & N_pos_theil_sen == 0, trend:= "negative probable",]
evap_trend_summary[N_neg_theil_sen > 0 & N_pos_theil_sen > 0, trend:= "uncertain",]

grid_cell_area <- unique(evap_trend_summary[, .(lon, lat)]) %>% grid_area() # m2
evap_trend_summary<- grid_cell_area[evap_trend_summary, on = .(lon, lat)]

land_area <- evap_trend_summary[,sum(area)]
uncertain_area <- evap_trend_summary[ trend == "uncertain", sum(area)]
notrend_area <- evap_trend_summary[trend == "no trend", sum(area)]

uncertain_area/land_area
notrend_area/land_area

### Merge data ----
evap_trend_lon_lat <- merge(evap_trend_summary, evap_trend_dci)

## Save data ----
saveRDS(evap_trend_lon_lat, paste0(PATH_SAVE_EVAP_TREND, "global_grid_slope_indices.rds"))

## Plot results ----
### DCI ----
evap_trend_dci[, DCI_theil_sen_brks := cut(DCI_theil_sen, breaks = c(-1,-0.6, -0.2, 0.2, 0.6, 1))]

cols_div <- c("darkblue","steelblue1","gray80", "darkorange","darkred")

ggplot(evap_trend_dci)+
  geom_tile(aes(x = lon, y = lat, fill = DCI_theil_sen_brks))+
  scale_fill_manual(values = cols_div) +
  labs(fill = "DCI")+
  theme_bw()
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "map_evap_trend_grid_DCI_theil_sen.png"), 
       width = 12, height = 8)


### Probability ----
ggplot(evap_trend_summary)+
  geom_tile(aes(x = lon, y = lat, fill = trend))+
  labs(fill = "Trend")+
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_bw()
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "map_evap_trend_probability_theil_sen.png"), 
       width = 12, height = 8)


ggplot(evap_trend_summary)+
  geom_tile(aes(x = lon, y = lat, fill = "trend"), col = "gray10")+
  geom_tile(data = evap_trend_summary[trend == "uncertain"], aes(x = lon, y = lat, fill = "uncertain"),  col = "deeppink1")+
  geom_tile(data = evap_trend_summary[trend == "no trend"], aes(x = lon, y = lat, fill = "no trend"),  col = "gray80")+
  scale_fill_manual(values = c("uncertain" = "deeppink1","no trend" = "gray80", "trend" = "gray10"))+
  labs(fill = "")+
  theme_bw()
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "map_evap_trend_uncertain_theil_sen.png"), 
       width = 12, height = 8)

### plot complete dataset only ----

ggplot(evap_trend_summary[count == 13,])+
  geom_tile(aes(x = lon, y = lat, fill = trend))+
  labs(fill = "Trend")+
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_bw()
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "map_evap_trend_probability_theil_sen_all_datasets.png"), 
       width = 12, height = 8)


ggplot(evap_trend_summary[count == 13,])+
  geom_tile(aes(x = lon, y = lat, fill = "trend"), col = "gray10")+
  geom_tile(data = evap_trend_summary[trend == "uncertain" & count == 13], aes(x = lon, y = lat, fill = "uncertain"),  col = "deeppink1")+
  geom_tile(data = evap_trend_summary[trend == "no trend"& count == 13], aes(x = lon, y = lat, fill = "no trend"),  col = "gray80")+
  scale_fill_manual(values = c("uncertain" = "deeppink1","no trend" = "gray80", "trend" = "gray10"))+
  labs(fill = "")+
  theme_bw()
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "map_evap_trend_uncertain_theil_sen_all_datasets.png"), 
       width = 12, height = 8)


## Analysis without JRA55 ----

evap_trend_dci_sel <- evap_trend[!(dataset == "jra55"),.(DCI_theil_sen = sum(theil_sen_slope/abs(theil_sen_slope)*significant_theil_sen, na.rm = TRUE)/.N,
                                                         DCI_lm = sum(lm_slope/abs(lm_slope)*significant_lm, na.rm = TRUE)/.N,
                                                         DCI_siegel = sum(siegel_slope/abs(siegel_slope)*significant_siegel, na.rm = TRUE)/.N
),
.(lon,lat)]

evap_trend_dci_sel <- evap_trend_dci_sel[complete.cases(evap_trend_dci_sel)]

evap_trend_pos <- evap_trend[significant_theil_sen == TRUE & theil_sen_slope > 0 & dataset != "jra55", .(N_pos_theil_sen = .N), .(lon,lat)]
evap_trend_neg <- evap_trend[significant_theil_sen == TRUE & theil_sen_slope < 0 & dataset != "jra55", .(N_neg_theil_sen = .N), .(lon,lat)]
evap_trend_ins <- evap_trend[significant_theil_sen == FALSE & dataset != "jra55", .(N_ins_theil_sen = .N), .(lon,lat)]

evap_trend_summary <- merge(evap_trend_pos, evap_trend_ins, by = c("lon", "lat"), all = TRUE)
evap_trend_summary <- merge(evap_trend_summary , evap_trend_neg, by = c("lon", "lat"), all = TRUE)

evap_trend_summary[is.na(N_pos_theil_sen), N_pos_theil_sen:= 0]
evap_trend_summary[is.na(N_neg_theil_sen), N_neg_theil_sen:= 0]
evap_trend_summary[is.na(N_ins_theil_sen), N_ins_theil_sen:= 0]

evap_trend_summary[, count:= sum(N_pos_theil_sen, N_neg_theil_sen, N_ins_theil_sen, na.rm = T),.(lon,lat)]
evap_trend_summary[, trend:= "no trend",]
evap_trend_summary[N_pos_theil_sen > 5& N_neg_theil_sen == 0, trend:= "positive likely",]
evap_trend_summary[N_pos_theil_sen > 1 & N_pos_theil_sen <= 5 & N_neg_theil_sen == 0, trend:= "positive probable",]

evap_trend_summary[N_neg_theil_sen > 5  & N_pos_theil_sen == 0, trend:= "negative likely",]
evap_trend_summary[N_neg_theil_sen > 1 & N_neg_theil_sen <= 5 & N_pos_theil_sen == 0, trend:= "negative probable",]
evap_trend_summary[N_neg_theil_sen > 0 & N_pos_theil_sen > 0, trend:= "uncertain",]

grid_cell_area <- unique(evap_trend_summary[, .(lon, lat)]) %>% grid_area() # m2
evap_trend_summary<- grid_cell_area[evap_trend_summary, on = .(lon, lat)]

land_area <- evap_trend_summary[,sum(area)]
uncertain_area <- evap_trend_summary[ trend == "uncertain", sum(area)]
notrend_area <- evap_trend_summary[trend == "no trend", sum(area)]

uncertain_area/land_area
notrend_area/land_area

#### stat only where all products are present ----
land_area_comp <- evap_trend_summary[count == 12, sum(area)] 
uncertain_area_comp <- evap_trend_summary[count == 12 & trend == "uncertain", sum(area)]
notrend_area_comp <- evap_trend_summary[count == 12 & trend == "no trend", sum(area)]

uncertain_area_comp/land_area_comp
notrend_area_comp/land_area_comp

### Merge data 

evap_trend_lon_lat_sel <- merge(evap_trend_summary, evap_trend_dci_sel)

## plot no jra 55

ggplot(evap_trend_dci_sel)+
  geom_tile(aes(x = lon, y = lat, fill = DCI_theil_sen_brks))+
  scale_fill_manual(values = cols_div) +
  labs(fill = "DCI")+
  theme_bw()
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "map_evap_trend_grid_DCI_theil_sen_nojra55.png"), 
       width = 12, height = 8)