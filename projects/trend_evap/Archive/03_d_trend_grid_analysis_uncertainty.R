# Create probability maps  ----
source('source/evap_trend.R')
source('source/graphics.R')
source('source/geo_functions.R')

## Data ----
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_evap_trend.rds"))  

## Analysis ----
evap_trend_pos <- evap_trend[significant_theil_sen == TRUE & theil_sen_slope > 0, .(N_pos_theil_sen = .N), .(lon,lat)]
evap_trend_neg <- evap_trend[significant_theil_sen == TRUE & theil_sen_slope < 0, .(N_neg_theil_sen = .N), .(lon,lat)]
evap_trend_ins <- evap_trend[significant_theil_sen == FALSE, .(N_ins_theil_sen = .N), .(lon,lat)]

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

## Save data ----
saveRDS(evap_trend_summary, paste0(PATH_SAVE_EVAP_TREND, "global_grid_probability_groups.rds"))

#### stat only where all products are present ----
land_area_comp <- evap_trend_summary[count == 13, sum(area)] 
uncertain_area_comp <- evap_trend_summary[count == 13 & trend == "uncertain", sum(area)]
notrend_area_comp <- evap_trend_summary[count == 13 & trend == "no trend", sum(area)]

uncertain_area_comp/land_area_comp
notrend_area_comp/land_area_comp

## Plot results  ----
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
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "evap_trend_probability_theil_sen.png"), 
       width = 12, height = 8)


ggplot(evap_trend_summary)+
  geom_tile(aes(x = lon, y = lat, fill = "trend"), col = "gray10")+
  geom_tile(data = evap_trend_summary[trend == "uncertain"], aes(x = lon, y = lat, fill = "uncertain"),  col = "red")+
  geom_tile(data = evap_trend_summary[trend == "no trend"], aes(x = lon, y = lat, fill = "no trend"),  col = "gray80")+
  scale_fill_manual(values = c("uncertain" = "deeppink1","no trend" = "gray80", "trend" = "gray10"))+
  labs(fill = "")+
  theme_bw()
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "evap_trend_uncertain_theil_sen.png"), 
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
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "evap_trend_probability_theil_sen_all_datasets.png"), 
       width = 12, height = 8)


ggplot(evap_trend_summary[count == 13,])+
  geom_tile(aes(x = lon, y = lat, fill = "trend"), col = "gray10")+
  geom_tile(data = evap_trend_summary[trend == "uncertain" & count == 13], aes(x = lon, y = lat, fill = "uncertain"),  col = "red")+
  geom_tile(data = evap_trend_summary[trend == "no trend"& count == 13], aes(x = lon, y = lat, fill = "no trend"),  col = "gray80")+
  scale_fill_manual(values = c("uncertain" = "deeppink1","no trend" = "gray80", "trend" = "gray10"))+
  labs(fill = "")+
  theme_bw()
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "evap_trend_uncertain_theil_sen_all_datasets.png"), 
       width = 12, height = 8)

## Analysis without JRA55 ----
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
