# Figure 3 - results depend product selection ----
## Opposing map
## N insig vs N sig map
source('source/evap_trend.R')
source('source/geo_functions.R')

library(rnaturalearth)

# Data
evap_signal <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_ranked_datasets_signal_booster_p_thresholds_bootstrap.rds"))
evap_opposers <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_ranked_datasets_opposing_p_thresholds_bootstrap.rds"))
evap_index <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_DCI_trend_groups_p_thresholds_bootstrap.rds"))


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

fig_signal_none <- ggplot(no_trenders[rank_datasets < 6])+
  geom_tile(aes(x = rank_datasets, y = variable, fill = dataset))+
  scale_fill_manual(values = cols_data)+
  labs(x = "Top no trenders", fill = "Dataset")+
  theme_bw()

fig_signal_pos <- ggplot(pos_signal[rank_datasets < 6])+
  geom_tile(aes(x = rank_datasets, y = variable, fill = dataset))+
  scale_fill_manual(values = cols_data)+
  labs(x = "Top Positiive\nSignal Boosters", fill = "Dataset")+
  theme_bw()

fig_signal_neg <- ggplot(neg_signal[rank_datasets < 6])+
  geom_tile(aes(x = rank_datasets, y = variable, fill = dataset))+
  scale_fill_manual(values = cols_data)+
  labs(x = "Top Negative\nSignal Boosters", fill = "Dataset")+
  theme_bw()


fig_opposers <- ggplot(evap_opposers)+
  geom_tile(aes(x = 1, y = variable, fill = dataset_leftout))+
  geom_tile(data = evap_opposers[rank_opp < 6], aes(x = rank_opp, y = variable, fill = dataset_leftout))+
  scale_fill_manual(values = cols_data)+
  labs(x = "Top opposers", fill = "Dataset")+
  theme_bw()


ggarrange(fig_opposers, fig_signal_pos, fig_signal_neg, fig_signal_none, align = "hv",
          common.legend = T, nrow = 1, ncol = 4)

# Map preparation -----
## World and Land borders ----
PATH_SAVE_PARTITION_EVAP <- paste0(PATH_SAVE, "partition_evap/")
load(paste0(PATH_SAVE_PARTITION_EVAP, "paths.Rdata"))

earth_box <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP_SPATIAL,
                            "earth_box.rds")) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")
world_sf <- ne_countries(returnclass = "sf")

## Labels ----
labs_y <- data.frame(lon = -170, lat = c(55, 25, -5, -35, -65))
labs_y_labels <- seq(60, -60, -30)
labs_y$label <- ifelse(labs_y_labels == 0, "°", ifelse(labs_y_labels > 0, "°N", "°S"))
labs_y$label <- paste0(abs(labs_y_labels), labs_y$label)
labs_y <- st_as_sf(labs_y, coords = c("lon", "lat"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")

labs_x <- data.frame(lon = seq(120, -120, -60), lat = -82)
labs_x$label <- ifelse(labs_x$lon == 0, "°", ifelse(labs_x$lon > 0, "°E", "°W"))
labs_x$label <- paste0(abs(labs_x$lon), labs_x$label)
labs_x <- st_as_sf(labs_x, coords = c("lon", "lat"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")


### Maps of p-value when grid becomes opposing ----

evap_index[p_val_opposing == "1", p_val_opposing := "no opposing trends"]
evap_index[p_val_opposing == ">0.2", p_val_opposing := "all"]
evap_index[p_val_opposing == "<=0.01", p_val_opposing := "p < 0.01"]
evap_index[p_val_opposing == "<=0.05", p_val_opposing := "p < 0.05"]
evap_index[p_val_opposing == "<=0.1", p_val_opposing := "p < 0.1"]
evap_index[p_val_opposing == "<=0.2", p_val_opposing := "p < 0.2"]

evap_index[, p_val_opposing := as.factor(p_val_opposing)]

evap_index[, p_val_opposing := factor(p_val_opposing, levels = 
                                        c("p < 0.01", "p < 0.05", "p < 0.1", "p < 0.2", "all", "no opposing trends"))]

to_plot_sf <- evap_index[, .(lon, lat, p_val_opposing)
][, value := as.numeric(p_val_opposing)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

cols_opposing <- c("darkblue","darkorchid3","darkorchid1","lightcoral","gold", "green")

fig_map_opposing <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = as.factor(value), fill = as.factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = cols_opposing, labels = levels(evap_index$p_val_opposing)) +
  scale_color_manual(values = cols_opposing,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Significant Trends\nare Opposing") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray20", size = 3) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray20", size = 3) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_EXPLORE, "map_p_val_opposing_bootstrap.png"), 
       width = 12, height = 8)

evap_index <- grid_cell_area[evap_index, on = .(lon, lat)]
p_val_opposing <- evap_index[,.(p_area = sum(area)), (p_val_opposing)]
p_val_opposing[, total_area := sum(p_area)]
p_val_opposing[, fraction := p_area/total_area]

p_val_opposing
p_val_opposing[order(p_val_opposing)]


## When trends become significant

evap_index[N_none_0_2 >= 7, more_sig_trends := "never" ]
evap_index[N_none_0_2 < 7, more_sig_trends := "p < 0.2" ]
evap_index[N_none_0_1 < 7, more_sig_trends := "p < 0.1" ]
evap_index[N_none_0_05 < 7, more_sig_trends := "p < 0.05" ]
evap_index[N_none_0_01 < 7, more_sig_trends := "p < 0.01" ]
evap_index[, more_sig_trends := as.factor(more_sig_trends) ]

to_plot_sf <- evap_index[, .(lon, lat, more_sig_trends)
][, value := as.numeric(more_sig_trends)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

cols_sig_trends <- c("gold", "darkblue","darkorchid3","darkorchid1","lightcoral")

fig_map_more_sig_trends <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = as.factor(value), fill = as.factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = cols_sig_trends, labels = levels(evap_index$more_sig_trends)) +
  scale_color_manual(values = cols_sig_trends,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "More Trends\nare Significant") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray20", size = 3) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray20", size = 3) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_EXPLORE, "map_p_val_more_sig_trends_bootstrap.png"), 
       width = 12, height = 8)

sig_trends <- evap_index[,.(p_area = sum(area)), (more_sig_trends)]
sig_trends[, total_area := sum(p_area)]
sig_trends[, fraction := p_area/total_area]

sig_trends
sig_trends[order(sig_trends)]


ggarrange(fig_map_more_sig_trends,
          fig_map_opposing, align = "hv", labels = c("a", "b"), nrow = 2, ncol = 1)

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_MAIN, "fig3_maps_opposing_products_bootstrap.png"), 
       width = 12, height = 12)
