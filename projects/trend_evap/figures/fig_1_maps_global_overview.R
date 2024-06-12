# Figure 1 - the global overview ----
source('source/evap_trend.R')
source('source/geo_functions.R')

library(rnaturalearth)

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

## Global map of ensemble trend  ----
### Data ----
### Input Data generated in projects/trend_evap/bootstrap/01_a
### Input Data generated in projects/trend_evap/bootstrap/01_b
evap_annual_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "evap_annual_trend_bootstrap.rds"))  
evap_annual_trend[p > 0.1 , trend_direction_v2 := "p > 0.1"]
evap_annual_trend[slope > 0 & p <= 0.1 , trend_direction_v2 := "positive p <= 0.1"]
evap_annual_trend[slope > 0 & p < 0.05 , trend_direction_v2 := "positive p < 0.05"]
evap_annual_trend[slope > 0 & p < 0.01 , trend_direction_v2 := "positive p < 0.01"]
evap_annual_trend[slope < 0 & p <= 0.1 , trend_direction_v2 := "negative p <= 0.1"]
evap_annual_trend[slope < 0 & p < 0.05 , trend_direction_v2 := "negative p < 0.05"]
evap_annual_trend[slope < 0 & p < 0.01 , trend_direction_v2 := "negative p < 0.01"]
## Table of range of trends ----
evap_annual_trend[, .(dataset, slope)][order(-slope),]

fig_trend <- ggplot(evap_annual_trend)+
  geom_segment(aes(x = dataset, y = lower, yend = upper))+
  geom_point(aes(x = dataset, y = slope, col = as.factor(trend_direction_v2)), size = 4)+
  geom_abline(intercept = 0, slope = 0, col = "black")+
  scale_color_manual(values = c("p > 0.1" = "gray", 
                               "negative p <= 0.1" = "royalblue1", 
                               "positive p < 0.01" = "darkred",
                               "positive p < 0.05" = "firebrick3",
                               "positive p <= 0.1" = "lightcoral"))+
  labs(y = expression(paste("ET trend [mm year"^-2,"]")), color = "Trend significance", x = "Dataset")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))


Q25_global <- evap_annual_trend[, quantile(slope, 0.25)]
Q75_global <- evap_annual_trend[, quantile(slope, 0.75)]
Q75_global/Q25_global

## folds of Q25 Q75 ----

evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_per_dataset_evap_slope_bootstrap.rds"))  
evap_trend_min_max <- evap_trend[dataset_count >= 12,.(max = max(slope), min = min(slope),
                                                       Q75 = quantile(slope, 0.75), Q25 = quantile(slope, 0.25)), .(lat,lon)]


evap_trend_min_max[abs(Q25) > Q75, fold := abs(Q25)/abs(Q75)]
evap_trend_min_max[abs(Q25) <= Q75, fold := abs(Q75)/abs(Q25)]
evap_trend_min_max[, fold_brk := cut(fold, breaks = c(1, 3.4, Inf))]
evap_trend_min_max[, fold_brk_detailed := cut(fold, breaks = c(1, 2, 4, 8, Inf))]


### Map ----

cols_grid_fold <- c("royalblue3","gold")
                                


to_plot_sf <- evap_trend_min_max[, .(lon, lat, fold_brk)
][, value := as.numeric(fold_brk)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_Q75Q25_fold <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = as.factor(value), fill = as.factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = cols_grid_fold, labels = levels(evap_trend_min_max$fold_brk)) +
  scale_color_manual(values = cols_grid_fold,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Quartile   \nfold [-]") +
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
        legend.title = element_text(size = 16),
        legend.position = "bottom")+
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_EXPLORE, "map_evap_folds_Q75_Q25.png"), 
       width = 15, height = 10)


## Quartile sign disagreement

evap_trend_min_max[Q75/Q25 < 0 , sign := "different sign"]
evap_trend_min_max[Q75/Q25 >= 0 , sign := "same sign" ]
evap_trend_min_max[, sign := as.factor(sign)]

cols_grid_agreement <- c("gold", "royalblue3")

to_plot_sf <- evap_trend_min_max[, .(lon, lat, sign)
][, value := as.numeric(sign)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_sign_agreement <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = as.factor(value), fill = as.factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = cols_grid_agreement, labels = levels(evap_trend_min_max$sign)) +
  scale_color_manual(values = cols_grid_agreement,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Q25 and Q75   \nsigns") +
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
        legend.title = element_text(size = 16),
        legend.position = "bottom")+
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))


ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_EXPLORE, "map_sign_agreement_Q75_Q25.png"), 
       width = 12, height = 8)

## ggarrange ----

ggmaps <- ggarrange(fig_Q75Q25_fold, fig_sign_agreement, align = "hv", 
                    ncol = 2, nrow = 1, labels = c("b", "c"))
ggarrange(fig_trend, ggmaps, align = "hv", 
          ncol = 1, nrow = 2, labels = c("a", ""), heights = c(0.6, 1.0), widths = c(0.3, 1))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_MAIN, "fig1_maps_evap_trend_overview.png"), 
       width = 12, height = 8)

## Stats for text ----

### Area of positive and negative trends

#### Ensemble ----
# grid_cell_area <- unique(evap_trend_grid_ensemble[, .(lon, lat)]) %>% grid_area() # m2
# evap_trend_grid_ensemble <- grid_cell_area[evap_trend_grid_ensemble, on = .(lon, lat)]
# 
# total_area <- evap_trend_grid_ensemble[, sum(area)]
# pos_trend <- evap_trend_grid_ensemble[slope >= 0, sum(area)]
# neg_trend <- evap_trend_grid_ensemble[slope < 0, sum(area)]
# 
# pos_area_fraction <- pos_trend/total_area
# neg_area_fraction <- neg_trend/total_area
# 
# pos_trend_1 <- evap_trend_grid_ensemble[slope_percent >= 1, sum(area)]
# neg_trend_1 <- evap_trend_grid_ensemble[slope_percent < -1, sum(area)]
# 
# pos_area_fraction_more_than1 <- pos_trend_1/total_area
# neg_area_fraction_more_than1 <- neg_trend_1/total_area

#### Folds ----
grid_cell_area <- unique(evap_trend_min_max [, .(lon, lat)]) %>% grid_area() # m2
evap_trend_min_max <- grid_cell_area[evap_trend_min_max, on = .(lon, lat)]
total_area <- evap_trend_min_max[, sum(area)]
evap_trend_min_max[, sum(area)/total_area, .(fold_brk)]

#### sign difference

evap_trend_min_max[, sum(area)/total_area, .(sign)]


## SI figures

### Map showing Max/Min ----
evap_trend_min_max[abs(min) > abs(max), max_direction := "negative"]
evap_trend_min_max[abs(min) <= abs(max), max_direction := "positive"]

evap_trend_min_max[, slope_ratio := max/min]
evap_trend_min_max[, IQR := Q75-Q25]

evap_trend_min_max[, slope_ratio_brk := cut(slope_ratio, breaks = c(-Inf,-20, -10, -2, 2, 10, 20, Inf))]
evap_trend_min_max[, IQR_brk := cut(IQR, breaks = c(0, 1, 2, 3, 4, 5, 21))]

#### IQR ----
cols_IQR <- c("lightblue", "steelblue1", "steelblue3","darkblue", "darkorange" ,"gold")

to_plot_sf <- evap_trend_min_max[, .(lon, lat, IQR_brk)
][, value := as.numeric(IQR_brk)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_IQR <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = as.factor(value), fill = as.factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = cols_IQR, labels = levels(evap_trend_min_max$IQR_brk)) +
  scale_color_manual(values = cols_IQR,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "IQR\n[mm/year/year]") +
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


#### Slope ratio ----

cols_slope_ratio <- c("darkblue", 
                                "steelblue3",
                                "steelblue1", 
                                "gray90",
                                "gold",
                                "darkorange",
                                "darkred"
)

to_plot_sf <- evap_trend_min_max[, .(lon, lat, slope_ratio_brk)
][, value := as.numeric(slope_ratio_brk)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_slope_ratio <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = as.factor(value), fill = as.factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = cols_slope_ratio, labels = levels(evap_trend_min_max$slope_ratio_brk)) +
  scale_color_manual(values = cols_slope_ratio,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Slope max/min [-]") +
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



