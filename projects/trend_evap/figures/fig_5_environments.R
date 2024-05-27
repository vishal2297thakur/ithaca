# Trend direction based on environment ----
source('source/evap_trend.R')
source('source/geo_functions.R')

## Data ----
### Input Data generated in projects/partition_evap/04
PATH_SAVE_PARTITION_EVAP <- paste0(PATH_SAVE, "partition_evap/")
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
### Input Data generated in projects/trend_evap/bootstrap/01_b
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_bootstrap.rds"))  

### Generated in projects/trend_evap/bootstrap/01_g
evap_index <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_DCI_trend_groups_p_thresholds_bootstrap.rds"))

### Mean trend ----
land_cover_class_trends <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "land_cover_mean_slope.rds"))

### DCI
land_cover <- merge(evap_mask[, .(lat, lon, land_cover_short_class)], 
                                    evap_index[, .(lon, lat, DCI_0_01, DCI_0_05, DCI_0_1, DCI_0_2, DCI_all)], 
                                    by = c("lon", "lat"))

land_cover  <- land_cover[land_cover_short_class != "Other"]

setnames(land_cover, old = c("DCI_0_01","DCI_0_05","DCI_0_1", "DCI_0_2", "DCI_all"), 
         new = c("p < 0.01", " p < 0.05", "p < 0.1", "p < 0.2", "all"))

data_sel_melt <- melt(land_cover, 
                      measure.vars = c("p < 0.01", " p < 0.05", "p < 0.1", "p < 0.2", "all"), 
                      id.vars = c("lon", "lat", "land_cover_short_class"))

data_sel_melt[, DCI_brk := cut(value, c(-1.01, -0.5,-0.07, 0.07,0.5,1))]
grid_cell_area <- unique(data_sel_melt[, .(lon, lat)]) %>% grid_area() # m2
data_sel_melt <- grid_cell_area[data_sel_melt, on = .(lon, lat)]

data_dci_area <- data_sel_melt[, .(DCI_area = sum(area)), .(DCI_brk, variable, land_cover_short_class)]
data_dci_area[, total_area := sum(DCI_area), .(variable, land_cover_short_class)]
data_dci_area[, fraction := DCI_area/total_area]
data_dci_area[DCI_brk == "(-1.01,-0.5]", DCI_brk := "[-1,-0.5]"]
data_dci_area[, DCI_brk:= factor(DCI_brk, levels = c("[-1,-0.5]", 
                                                     "(-0.5,-0.07]", "(-0.07,0.07]", "(0.07,0.5]",
                                                      "(0.5,1]"))]


#### Figure 1 gg ----

fig_DCI <- ggplot(data_dci_area)+
  geom_bar(aes(x = variable, y = fraction, fill = DCI_brk), stat = "identity")+
  theme_bw()+
  scale_fill_manual(values = c("darkblue",  "lightblue", "gray90", "orange","darkred"))+
  labs(y = "Area Fraction", x = "P-value Threshold", fill = "DCI")+
  facet_wrap(~land_cover_short_class, ncol = 1)+
  theme(legend.position="bottom")+guides(fill = guide_legend(nrow = 3 ,byrow = TRUE))

## Trend direction

### Figure 2 trend direction ----
#### Figure 2 prep ----

land_cover <- merge(evap_mask[, .(lat, lon, land_cover_short_class)], 
                    evap_index[, .(lon, lat, trend_0_01, trend_0_05, trend_0_1, trend_0_2, trend_all)], 
                    by = c("lon", "lat"))

land_cover  <- grid_cell_area[land_cover, on = .(lon, lat)]

land_cover  <- land_cover[land_cover_short_class != "Other"]


setnames(land_cover, old = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all"), 
         new = c("p < 0.01", " p < 0.05", "p < 0.1", "p < 0.2", "all"))

data_sel_trend_melt <- melt(land_cover, measure.vars = c("p < 0.01", " p < 0.05", "p < 0.1", "p < 0.2", "all"))

data_sel_trend_area <- data_sel_trend_melt[, .(trend_area = sum(area)), .(value, variable, land_cover_short_class)] 
data_sel_trend_area[, total_area := sum(trend_area), .(variable, land_cover_short_class)]
data_sel_trend_area[, fraction := trend_area/total_area]

#### Figure 2 gg ----

fig_trend <- ggplot(data_sel_trend_area)+
  geom_bar(aes(x = variable, y = fraction, fill = value), stat = "identity")+
  scale_fill_manual(values = c ("darkblue", "gray90", "yellow","darkred"))+
  theme_bw()+
  labs(x = "P-value Threshold", y = "Area fraction [-]", fill = "Direction of\nSignificant\nTrends")+
  theme(legend.position="bottom")+
  facet_wrap(~land_cover_short_class, ncol = 1)+
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))


### Significant trends 

evap_index[, N_sum_0_01:= N_pos_0_01+N_neg_0_01]
evap_index[, N_sum_0_05:= N_pos_0_05+N_neg_0_05]
evap_index[, N_sum_0_1:= N_pos_0_1+N_neg_0_1]
evap_index[, N_sum_0_2:= N_pos_0_2+N_neg_0_2]
evap_index[, N_sum_all:= N_pos_all+N_neg_all]

land_cover <- merge(evap_mask[, .(lat, lon, land_cover_short_class)], 
                    evap_index[, .(lon, lat, N_sum_0_01, N_sum_0_05, N_sum_0_1, N_sum_0_2, N_sum_all)], 
                    by = c("lon", "lat"))

land_cover  <- grid_cell_area[land_cover, on = .(lon, lat)]

land_cover  <- land_cover[land_cover_short_class != "Other"]

setnames(land_cover , old = c("N_sum_0_01", "N_sum_0_05", 
                            "N_sum_0_1", "N_sum_0_2", "N_sum_all"), 
         new = c("p < 0.01", " p < 0.05", "p < 0.1", "p < 0.2", "all"))

data_melt <- melt(land_cover, measure.vars = c("p < 0.01", " p < 0.05", "p < 0.1", "p < 0.2", "all"))

data_melt[, N_sum_brk := cut(round(value), c(-0.1, 0.9, 4, 7, 11, 14))]
data_melt[N_sum_brk == "(-0.1,0.9]", N_sum_brk := "[0,1)"]
data_melt[N_sum_brk == "(0.9,4]", N_sum_brk := "[1,4]"]

N_sig_area <- data_melt[,.(N_sig_area = sum(area)), .(N_sum_brk, variable, land_cover_short_class)]
N_sig_area[, variable_area := sum(N_sig_area), .(variable, land_cover_short_class)]
N_sig_area[, fraction := N_sig_area/variable_area]
N_sig_area[, N_sum_brk:= factor(N_sum_brk, levels = c("[0,1)", "[1,4]",
                                                      "(4,7]", 
                                                      "(7,11]","(11,14]"))]

fig_Ntrend <- ggplot(N_sig_area)+
  geom_bar(aes(x = variable, y = fraction, fill = N_sum_brk), stat = "identity")+
  theme_bw()+
  labs( fill = "Number of\nSignificant\nTrends", x = 'P-value Threshold', y = "Area Fraction [-]")+
  scale_fill_manual(values = c("gray90", "lightblue", "steelblue1", "royalblue3", "darkblue"))+
  theme(legend.position="bottom")+
  facet_wrap(~land_cover_short_class, ncol = 1)+
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))


ggarrange(fig_Ntrend, fig_trend, fig_DCI, align = "hv",
          ncol = 3, nrow = 1,
          labels = c("a", "b", "c"))
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_MAIN, "fig5_land_use_area_fraction_trend_indices_p_val_threshold_bootstrap.png"), 
       width = 12, height = 10)


## Signal booster - land use

# Data
evap_signal <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "land_use_ranked_datasets_signal_booster_p_thresholds_bootstrap.rds"))
evap_opposers <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "land_use_datasets_opposing_p_thresholds_bootstrap.rds"))

evap_signal <- evap_signal[land_cover_short_class != "Other"]
evap_opposers <- evap_opposers[land_cover_short_class != "Other"]
#### Ranked data products

no_trenders <- evap_signal[variable %in%
                             c("sum_N_none_0_2", "sum_N_none_0_1", "sum_N_none_0_05", "sum_N_none_0_01")]

no_trenders[variable == "sum_N_none_0_01", variable := "p < 0.01", ]
no_trenders[variable == "sum_N_none_0_05", variable := "p < 0.05", ]
no_trenders[variable == "sum_N_none_0_1", variable := "p < 0.1", ]
no_trenders[variable == "sum_N_none_0_2", variable := "p < 0.2", ]

pos_signal <- evap_signal[variable %in%
                            c("sum_N_pos_all", "sum_N_pos_0_2", "sum_N_pos_0_1", "sum_N_pos_0_05", "sum_N_pos_0_01")]

pos_signal[variable == "sum_N_pos_0_01", variable := "p < 0.01", ]
pos_signal[variable == "sum_N_pos_0_05", variable := "p < 0.05", ]
pos_signal[variable == "sum_N_pos_0_1", variable := "p < 0.1", ]
pos_signal[variable == "sum_N_pos_0_2", variable := "p < 0.2", ]
pos_signal[variable == "sum_N_pos_all", variable := "all", ]


neg_signal <- evap_signal[variable %in%
                            c("sum_N_neg_all", "sum_N_neg_0_2", "sum_N_neg_0_1", "sum_N_neg_0_05", "sum_N_neg_0_01")]

neg_signal[variable == "sum_N_neg_0_01", variable := "p < 0.01", ]
neg_signal[variable == "sum_N_neg_0_05", variable := "p < 0.05", ]
neg_signal[variable == "sum_N_neg_0_1", variable := "p < 0.1", ]
neg_signal[variable == "sum_N_neg_0_2", variable := "p < 0.2", ]
neg_signal[variable == "sum_N_neg_all", variable := "all", ]

fig_signal_none <- ggplot(no_trenders[rank_datasets <= 5])+
  geom_tile(aes(x = rank_datasets, y = variable, fill = dataset))+
  scale_fill_manual(values = cols_data)+
  labs(x = "Top no trenders", fill = "Dataset", y = "")+
  facet_wrap(~land_cover_short_class, ncol = 1)+
  theme_minimal()

fig_signal_pos <- ggplot(pos_signal[rank_datasets < 6])+
  geom_tile(aes(x = rank_datasets, y = variable, fill = dataset))+
  scale_fill_manual(values = cols_data)+
  labs(x = "Top Positiive\nSignal Boosters", fill = "Dataset", y = "")+
  facet_wrap(~land_cover_short_class, ncol = 1)+
  theme_minimal()

fig_signal_neg <- ggplot(neg_signal[rank_datasets < 6])+
  geom_tile(aes(x = rank_datasets, y = variable, fill = dataset))+
  scale_fill_manual(values = cols_data)+
  labs(x = "Top Negative\nSignal Boosters", fill = "Dataset", y = "")+
  facet_wrap(~land_cover_short_class, ncol = 1)+
  theme_minimal()


fig_opposers <- ggplot(evap_opposers)+
  geom_tile(aes(x = 1, y = variable, fill = dataset_leftout))+
  geom_tile(data = evap_opposers[rank_opp <= 5], aes(x = rank_opp, y = variable, fill = dataset_leftout))+
  scale_fill_manual(values = cols_data)+
  labs(x = "Top opposers", fill = "Dataset", y = "")+
  facet_wrap(~land_cover_short_class, ncol = 1)+
  theme_minimal()


ggarrange(fig_opposers, fig_signal_pos, fig_signal_neg, fig_signal_none, align = "hv",
          common.legend = T, legend = "right" ,nrow = 1, ncol = 4, labels = c("a", "b", "c", "d"))


ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_MAIN, "fig6_product_signals_bootstrap_landcover.png"), 
       width = 12, height = 10)
