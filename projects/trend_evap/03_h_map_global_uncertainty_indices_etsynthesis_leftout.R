# Map of DCI and probability groups
source('source/evap_trend.R')

## Data ----
### Input data generated in 01_e
evap_trend<- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_uncertainty_dataset_leftout.rds"))

evap_trend_no_etsynthesis <- evap_trend[dataset_leftout == "etsynthesis"]

## Plot results ----
### Probability groups ----
ggplot(evap_trend_no_etsynthesis)+
  geom_tile(aes(x = lon, y = lat, fill = trend))+
  labs(fill = "Trend")+
  scale_fill_manual(values = c("negative likely" = "darkblue", 
                               "negative probable" = "steelblue1", 
                               "positive likely" = "darkred", 
                               "positive probable" = "darkorange", 
                               "uncertain" = "deeppink1", 
                               "no trend" = "gray80"))+
  theme_bw()
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "map_evap_trend_probability_theil_sen_no_etsynthesis.png"), 
       width = 12, height = 8)


ggplot(evap_trend_no_etsynthesis)+
  geom_tile(aes(x = lon, y = lat, fill = "trend"), col = "gray10")+
  geom_tile(data = evap_trend_no_etsynthesis[trend == "uncertain"], aes(x = lon, y = lat, fill = "uncertain"),  col = "deeppink1")+
  geom_tile(data = evap_trend_no_etsynthesis[trend == "no trend"], aes(x = lon, y = lat, fill = "no trend"),  col = "gray80")+
  scale_fill_manual(values = c("uncertain" = "deeppink1","no trend" = "gray80", "trend" = "gray10"))+
  labs(fill = "")+
  theme_bw()
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "map_evap_trend_uncertain_theil_sen_no_etsynthesis.png"), 
       width = 12, height = 8)

