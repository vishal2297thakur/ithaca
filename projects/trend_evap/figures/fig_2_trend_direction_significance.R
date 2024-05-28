# Figure 3 - results depend product selection ----
## Opposing map
## N insig vs N sig map
source('source/evap_trend.R')
source('source/geo_functions.R')

library(rnaturalearth)

# Data
evap_index <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_DCI_trend_groups_p_thresholds_bootstrap.rds"))

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


## Maps of p-value when grid becomes opposing ----

evap_index[p_val_opposing == "1", p_val_opposing := "no opposing\ntrends"]
evap_index[p_val_opposing == ">0.2", p_val_opposing := "p <= 1"]
evap_index[p_val_opposing == "<=0.01", p_val_opposing := "p < 0.01"]
evap_index[p_val_opposing == "<=0.05", p_val_opposing := "p < 0.05"]
evap_index[p_val_opposing == "<=0.1", p_val_opposing := "p < 0.1"]
evap_index[p_val_opposing == "<=0.2", p_val_opposing := "p < 0.2"]

evap_index[, p_val_opposing := as.factor(p_val_opposing)]

evap_index[, p_val_opposing := factor(p_val_opposing, levels = 
                                        c("p < 0.01", "p < 0.05", "p < 0.1", "p < 0.2", "p <= 1", "no opposing\ntrends"))]

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
  labs(x = NULL, y = NULL, fill = "Significant \nTrends\nare Opposing") +
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

grid_cell_area <- unique(evap_index[, .(lon, lat)]) %>% grid_area() # m2
evap_index <- grid_cell_area[evap_index, on = .(lon, lat)]
p_val_opposing <- evap_index[,.(p_area = sum(area)), (p_val_opposing)]
p_val_opposing[, total_area := sum(p_area)]
p_val_opposing[, fraction := p_area/total_area]

p_val_opposing
p_val_opposing[order(p_val_opposing)]


## When more trends become significant ----

evap_index[N_none_0_2 >= 7, more_sig_trends := "p <= 1" ]
evap_index[N_none_0_2 < 7, more_sig_trends := "p < 0.2" ]
evap_index[N_none_0_1 < 7, more_sig_trends := "p < 0.1" ]
evap_index[N_none_0_05 < 7, more_sig_trends := "p < 0.05" ]
evap_index[N_none_0_01 < 7, more_sig_trends := "p < 0.01" ]
evap_index[, more_sig_trends := as.factor(more_sig_trends) ]
evap_index[, more_sig_trends := factor(more_sig_trends, levels = 
                                        c("p < 0.01", "p < 0.05", "p < 0.1", "p < 0.2", "p <= 1"))]

to_plot_sf <- evap_index[, .(lon, lat, more_sig_trends)
][, value := as.numeric(more_sig_trends)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

cols_sig_trends <- c("darkblue","darkorchid3","darkorchid1","lightcoral", "gold")

fig_map_more_sig_trends <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = as.factor(value), fill = as.factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = cols_sig_trends, labels = levels(evap_index$more_sig_trends)) +
  scale_color_manual(values = cols_sig_trends,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Most Trends\nare \nSignificant") +
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

sig_trends <- evap_index[,.(p_area = sum(area)), (more_sig_trends)]
sig_trends[, total_area := sum(p_area)]
sig_trends[, fraction := p_area/total_area]

sig_trends
sig_trends[order(sig_trends)]

## Maps of DCI when all trends are considered ----

evap_index[, DCI_all_brk := cut(DCI_all, c(-1.01,-0.5,-0.07, 0.07,0.5,1))]
evap_index[DCI_all_brk == "(-1.01,-0.5]", DCI_all_brk := "[-1,-0.5]"]
evap_index[, DCI_all_brk:= factor(DCI_all_brk, levels = c("[-1,-0.5]", 
                                                     "(-0.5,-0.07]", "(-0.07,0.07]", "(0.07,0.5]",
                                                     "(0.5,1]"))]

to_plot_sf <- evap_index[, .(lon, lat, DCI_all_brk)
][, value := as.numeric(DCI_all_brk)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

cols_DCI <- c("darkblue", "lightblue", "gray90", "orange","darkred")

fig_map_DCI <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = as.factor(value), fill = as.factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = cols_DCI, labels = levels(evap_index$DCI_all_brk)) +
  scale_color_manual(values = cols_DCI,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "DCI\nall Trends") +
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

DCI_all <- evap_index[,.(p_area = sum(area)), (DCI_all_brk)]
DCI_all[, total_area := sum(p_area)]
DCI_all[, fraction := p_area/total_area]

DCI_all
DCI_all[order(DCI_all)]


## Barplot Figures -----
### Figure 1 DCI area fraction ----
#### Figure 1 prep ----

data_sel <- subset(evap_index, select = c("DCI_0_01","DCI_0_05","DCI_0_1", "DCI_0_2", "DCI_all", "lon", "lat"))
setnames(data_sel, old = c("DCI_0_01","DCI_0_05","DCI_0_1", "DCI_0_2", "DCI_all"), 
         new = c("p < 0.01", " p < 0.05", "p < 0.1", "p < 0.2", "p <= 1"))

data_sel_melt <- melt(data_sel, 
                      measure.vars = c("p < 0.01", " p < 0.05", "p < 0.1", "p < 0.2", "p <= 1"), 
                      id.vars = c("lon", "lat"))

data_sel_melt[, DCI_brk := cut(value, c(-1.01,-0.5,-0.07, 0.07,0.5,1))]
data_sel_melt <- grid_cell_area[data_sel_melt, on = .(lon, lat)]

data_dci_area <- data_sel_melt[, .(DCI_area = sum(area)), .(DCI_brk, variable)]
data_dci_area[, total_area := sum(DCI_area), variable]
data_dci_area[, fraction := DCI_area/total_area]
data_dci_area[DCI_brk == "(-1.01,-0.5]", DCI_brk := "[-1,-0.5]"]
data_dci_area[, DCI_brk:= factor(DCI_brk, levels = c("[-1,-0.5]", 
                                                     "(-0.5,-0.07]", "(-0.07,0.07]", "(0.07,0.5]",
                                                     "(0.5,1]"))]

#### Figure 1 gg ----

fig_DCI <- ggplot(data_dci_area)+
  geom_bar(aes(x = variable, y = fraction, fill = DCI_brk), stat = "identity")+
  theme_bw()+
  scale_fill_manual(values = c("darkblue", "lightblue", "gray90", "orange","darkred"))+
  labs(y = "Area Fraction [-]", x = "P-value Threshold", fill = "DCI")+
  theme(legend.position="right")+
  guides(fill = guide_legend(nrow = 5 ,byrow = TRUE))+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))


### Figure 2 trend direction ----
#### Figure 2 prep ----
data_sel_trend <- subset(evap_index, select = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all", "lat", "lon"))
data_sel_trend  <- grid_cell_area[data_sel_trend, on = .(lon, lat)]

setnames(data_sel_trend, old = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all"), 
         new = c("p < 0.01", " p < 0.05", "p < 0.1", "p < 0.2", "p <= 1"))

data_sel_trend_melt <- melt(data_sel_trend, measure.vars = c("p < 0.01", " p < 0.05", "p < 0.1", "p < 0.2", "p <= 1"))

data_sel_trend_area <- data_sel_trend_melt[, .(trend_area = sum(area)), .(value, variable)] 
data_sel_trend_area[, total_area := sum(trend_area), variable]
data_sel_trend_area[, fraction := trend_area/total_area]

#### Figure 2 gg ----

fig_trend <- ggplot(data_sel_trend_area)+
  geom_bar(aes(x = variable, y = fraction, fill = value), stat = "identity")+
  scale_fill_manual(values = c ("darkblue", "gray90", "yellow","darkred"))+
  theme_bw()+
  labs(x = "P-value Threshold", y = "Area fraction [-]", fill = "Direction of\nSignificant\nTrends")+
  theme(legend.position="right")+
  guides(fill = guide_legend(nrow = 4, byrow = TRUE))+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

### Figure 3 Number of significant trends over p ----
### Product selection: Trend or no trend

#### Figure 3 prep ----
evap_index[, N_sum_0_01:= N_pos_0_01+N_neg_0_01]
evap_index[, N_sum_0_05:= N_pos_0_05+N_neg_0_05]
evap_index[, N_sum_0_1:= N_pos_0_1+N_neg_0_1]
evap_index[, N_sum_0_2:= N_pos_0_2+N_neg_0_2]
evap_index[, N_sum_all:= N_pos_all+N_neg_all]

evap_sel <- subset(evap_index, select = c("N_sum_0_01", "N_sum_0_05", 
                                          "N_sum_0_1", "N_sum_0_2", "N_sum_all",
                                          "lat", "lon"))
evap_sel  <- grid_cell_area[evap_sel, on = .(lon, lat)]

setnames(evap_sel , old = c("N_sum_0_01", "N_sum_0_05", 
                            "N_sum_0_1", "N_sum_0_2", "N_sum_all"), 
         new = c("p < 0.01", " p < 0.05", "p < 0.1", "p < 0.2", "p <= 1"))

data_melt <- melt(evap_sel, measure.vars = c("p < 0.01", " p < 0.05", "p < 0.1", "p < 0.2", "p <= 1"))

data_melt[, N_sum_brk := cut(round(value), c(-0.1, 0.9, 4, 7, 11, 14))]
data_melt[N_sum_brk == "(-0.1,0.9]", N_sum_brk := "[0,1)"]
data_melt[N_sum_brk == "(0.9,4]", N_sum_brk := "[1,4]"]

N_sig_area <- data_melt[,.(N_sig_area = sum(area)), .(N_sum_brk, variable)]
N_sig_area[, variable_area := sum(N_sig_area), variable]
N_sig_area[, fraction := N_sig_area/variable_area]
N_sig_area[, N_sum_brk:= factor(N_sum_brk, levels = c("[0,1)", "[1,4]",
                                                      "(4,7]", 
                                                      "(7,11]","(11,14]"))]

fig_Ntrend <- ggplot(N_sig_area)+
  geom_bar(aes(x = variable, y = fraction, fill = N_sum_brk), stat = "identity")+
  theme_bw()+
  labs( fill = "Number of\nSignificant\nTrends", x = 'P-value Threshold', y = "Area Fraction [-]")+
  scale_fill_manual(values = c("gray90", "lightblue", "steelblue1", "royalblue3", "darkblue"))+
  theme(legend.position="right")+
  guides(fill = guide_legend(nrow = 5, byrow = TRUE))+
  theme(axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))


## Composite figure ----

maps <- ggarrange(fig_map_more_sig_trends,
                  fig_map_opposing, fig_map_DCI, align = "hv", labels = c("a", "c", "e"), nrow = 3, ncol = 1)

area_plots <- ggarrange(fig_Ntrend, fig_trend, fig_DCI, align = "hv",
          nrow = 3, ncol = 1,
          labels = c("b", "d", "f"))

fig_2 <- ggarrange(maps, area_plots, ncol = 2, widths = c(1, 0.75))
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_MAIN, "fig2_maps_barplots.png"), 
       width = 12, height = 8)
