# SI support for figure 3
source('source/evap_trend.R')
source('source/geo_functions.R')


## Data ----
### Input data generated in trend_evap/bootstrap/01_c 
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_bootstrap.rds"))  
evap_trend <- evap_trend[dataset_count >= 14]

grid_cell_area <- unique(evap_trend[, .(lon, lat)]) %>% grid_area() # m2
evap_trend <- grid_cell_area[evap_trend, on = .(lon, lat)]

evap_trend[slope >= 0 , trend_direction_detailed := "positive p <= 1"]
evap_trend[slope > 0 & p < 0.1 , trend_direction_detailed := "positive p < 0.1"]
evap_trend[slope > 0 & p < 0.05 , trend_direction_detailed := "positive p < 0.05"]
evap_trend[slope > 0 & p < 0.01 , trend_direction_detailed := "positive p < 0.01"]
evap_trend[slope < 0  , trend_direction_detailed := "negative p <= 1"]
evap_trend[slope < 0 & p < 0.1 , trend_direction_detailed := "negative p < 0.1"]
evap_trend[slope < 0 & p < 0.05 , trend_direction_detailed := "negative p < 0.05"]
evap_trend[slope < 0 & p < 0.01 , trend_direction_detailed := "negative p < 0.01"]
evap_trend[, trend_direction_detailed  := factor(trend_direction_detailed, 
                                                       level = c("positive p < 0.01", "positive p < 0.05", "positive p < 0.1", "positive p <= 1",
                                                                 "negative p <= 1", "negative p < 0.1", "negative p < 0.05",
                                                                  "negative p < 0.01"), ordered = T),]


evap_trend_area <- evap_trend[,.(trend_area = sum(area)),.(trend_direction_detailed, dataset)]
evap_trend_area <- evap_trend_area[complete.cases(evap_trend_area)]
evap_trend_area[, total_area:= sum(trend_area), .(dataset)]
evap_trend_area[, fraction:= trend_area/total_area]
saveRDS(evap_trend_area, paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_SI_fig_3_area_fraction_trend_significance_by_product.rds"))

ggplot(evap_trend_area)+
  geom_bar(aes(x = dataset, y = fraction, fill = trend_direction_detailed), stat = "identity") +
  xlab('Dataset')  +
  ylab('Area fraction [-]')  +
  labs(fill = 'Trend significance')  +
  scale_fill_manual(values = c(
    "negative p <= 1" = "lightblue",
    "negative p < 0.1" = "royalblue1", 
    "negative p < 0.05" = "royalblue3", 
    "negative p < 0.01" = "darkblue", 
    "positive p < 0.01" = "#330000",
    "positive p < 0.05" = "darkred",
    "positive p < 0.1" = "lightcoral",
    "positive p <= 1" = "orange"))+  theme_light() +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "SI_fig_3_support_evap_trend_direction_detailed_per_dataset_bootstrap.png"), 
       width = 12, height = 8)


## CSI BIAS ----
CSI_BIAS_data <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_CSI_BIAS_dataset_bootstrap.rds"))
CSI_BIAS_data[, BIAS_brks := cut(BIAS, breaks = c(1/6.02,1/4,1/2,1/1.1,1.1,2,4,6.02))]
CSI_BIAS_data[, CSI_brks := cut(CSI, breaks = c(1/1000,1/10,2/10,3/10,1))]
CSI_BIAS_data[CSI < 0.1, CSI_fac := "< 10 %"]
CSI_BIAS_data[CSI >= 0.1 & CSI < 0.2, CSI_fac := "< 20 %"]
CSI_BIAS_data[CSI >= 0.2 & CSI < 0.3, CSI_fac := "< 30 %"]
CSI_BIAS_data[CSI >= 0.3, CSI_fac := "> 30 %"]

fig_CSI <-ggplot(CSI_BIAS_data[CSI < 1])+
  geom_tile(aes(x = dataset_A , y = dataset_B, fill = CSI_fac))+
  scale_fill_manual(values = c("gold", "darkorange",  "lightcoral", "darkred"))+
  theme_bw()+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))+
  labs(x = "Dataset B", y = "Dataset A", fill = "Critical Success\nIndex")

fig_BIAS <- ggplot(CSI_BIAS_data[CSI < 1])+
  geom_tile(aes(x = dataset_A , y = dataset_B, fill = BIAS_brks))+
  scale_fill_manual(values = c("darkblue", "royalblue3", "lightblue", "gray90","orange","darkorange","darkred"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))+
  labs(x = "Dataset B", y = "Dataset A", fill = "Bias")

ggarrange(fig_CSI, fig_BIAS, align = "hv", labels = c("a", "b"))
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "SI_fig_3_support_CSI_BIAS.png"), 
       width = 12, height = 4)
