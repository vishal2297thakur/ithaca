# Calculate dataset concurrence index for each grid ----

source('source/evap_trend.R')
source('source/geo_functions.R')
source('source/graphics.R')

## Data ----
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_evap_trend.rds"))  

evap_trend <- evap_trend[complete.cases(evap_trend)]

## Analysis ----
evap_trend_dci <- evap_trend[,.(DCI_theil_sen = sum(theil_sen_slope/abs(theil_sen_slope)*significant_theil_sen, na.rm = TRUE)/.N,
                                   DCI_lm = sum(lm_slope/abs(lm_slope)*significant_lm, na.rm = TRUE)/.N,
                                   DCI_siegel = sum(siegel_slope/abs(siegel_slope)*significant_siegel, na.rm = TRUE)/.N
                                   ),
                                 .(lon,lat)]

evap_trend_dci_sel <- evap_trend[!(dataset == "jra55"),.(DCI_theil_sen = sum(theil_sen_slope/abs(theil_sen_slope)*significant_theil_sen, na.rm = TRUE)/.N,
                                DCI_lm = sum(lm_slope/abs(lm_slope)*significant_lm, na.rm = TRUE)/.N,
                                DCI_siegel = sum(siegel_slope/abs(siegel_slope)*significant_siegel, na.rm = TRUE)/.N
),
.(lon,lat)]

evap_trend_dci_sel <- evap_trend_dci_sel[complete.cases(evap_trend_dci_sel)]

## Plot results ----
evap_trend_dci[, DCI_theil_sen_brks := cut(DCI_theil_sen, breaks = c(-1,-0.6, -0.2, 0.2, 0.6, 1))]
evap_trend_dci[, DCI_lm_brks := cut(DCI_lm, breaks = c(-1,-0.6, -0.2, 0.2,0.6, 1))]

evap_trend_dci_sel[, DCI_theil_sen_brks := cut(DCI_theil_sen, breaks = c(-1.000001,-0.6, -0.2, 0.2, 0.6, 1))]
evap_trend_dci_sel[, DCI_lm_brks := cut(DCI_lm, breaks = c(-1.000001,-0.6, -0.2, 0.2,0.6, 1))]

cols_div <- c("darkblue","steelblue1","gray80", "darkorange","darkred")

ggplot(evap_trend_dci)+
  geom_tile(aes(x = lon, y = lat, fill = DCI_theil_sen_brks))+
  scale_fill_manual(values = cols_div) +
  labs(fill = "DCI")+
  theme_bw()
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "evap_trend_grid_DCI_theil_sen.png"), 
       width = 12, height = 8)

ggplot(evap_trend_dci)+
  geom_tile(aes(x = lon, y = lat, fill = DCI_lm_brks))+
  scale_fill_manual(values = cols_div) +
  labs(fill = "DCI")+
  theme_bw()
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "evap_trend_grid_DCI_lm.png"), 
       width = 12, height = 8)


ggplot(evap_trend_dci_sel)+
  geom_tile(aes(x = lon, y = lat, fill = DCI_theil_sen_brks))+
  scale_fill_manual(values = cols_div) +
  labs(fill = "DCI")+
  theme_bw()
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "evap_trend_grid_DCI_theil_sen_nojra55.png"), 
       width = 12, height = 8)
