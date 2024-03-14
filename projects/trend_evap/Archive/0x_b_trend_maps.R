# Plotting global maps with trend

source("source/evap_trend.R")
evap_trend<- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope.rds"))  

library(ggplot2)
evap_trend_camele <- evap_trend[dataset == "camele"]
evap_trend_camele[, slope_percent_brk := cut(slope_percent,c(-7, seq(-2.0,1.5,0.5),7))]

ggplot(evap_trend_camele)+
  geom_tile(aes(x = lon, y = lat, fill = slope_percent_brk))+
  scale_fill_manual(values = c("#228292", "#5c9e9c", "#91bca8", "#c7deb4","#ffffc1", "#e8cf8f", "#cfa163", "#b5773e", "#9d541f"))+
  ggtitle(label = "camele")+
  theme_bw()

evap_trend_camele[,summary(slope_percent)]
