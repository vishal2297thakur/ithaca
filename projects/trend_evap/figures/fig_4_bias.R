# figure 4
source('source/evap_trend.R')

## CSI BIAS ----
CSI_BIAS_data <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_CSI_BIAS_dataset_bootstrap.rds"))
CSI_BIAS_data[, BIAS_brks := cut(BIAS, breaks = c(round(1/6.1, 2),1/4,1/2,round(1/1.2, 2),1.2,2,4,6.02))]

CSI_BIAS_data[, CSI_brks := cut(CSI, breaks = c(1/1000,1/10,2/10,3/10,1))]
CSI_BIAS_data[CSI < 0.1, CSI_fac := "< 10 %"]
CSI_BIAS_data[CSI >= 0.1 & CSI < 0.2, CSI_fac := "< 20 %"]
CSI_BIAS_data[CSI >= 0.2 & CSI < 0.3, CSI_fac := "< 30 %"]
CSI_BIAS_data[CSI >= 0.3, CSI_fac := ">= 30 %"]

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
  labs(x = "Dataset B", y = "Dataset A", fill = "Critical\nSuccess Index")

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
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_MAIN, "fig4_CSI_BIAS.png"), 
       width = 12, height = 4)
