# SI support for figure 1 - the global overview ----
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

## Read data
evap_trend_stats <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_1_b_c_grid_quartile_stats.rds"))

evap_trend_stats[fold_brk == "(3.2,Inf]" & sign == "different sign", problem := "High variety & different sign"] 

evap_trend_stats[fold_brk == "(3.2,Inf]" & sign == "same sign", problem := "High variety & same sign"] 

evap_trend_stats[fold_brk == "(1,3.2]" & sign == "different sign" & abs(Q25) > 0.5 & abs(Q75) > 0.5, problem := "Low variety & different sign & large trend"] 

evap_trend_stats[fold_brk == "(1,3.2]" & sign == "same sign", problem := "Low variety & same sign"] 

evap_trend_stats[fold_brk == "(1,3.2]" & sign == "different sign" & (abs(Q25) < 0.5 | abs(Q75) < 0.5), problem := "Low variety & different sign & small trend"] 

evap_trend_stats[, problem:= as.factor(problem)]

### Map ----

cols_problem <- c("#330000","orange", "darkred", "lightblue", "darkblue")

to_plot_sf <- evap_trend_stats[, .(lon, lat, problem)
][, value := as.numeric(problem)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_problem <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = as.factor(value), fill = as.factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = cols_problem, labels = levels(evap_trend_stats$problem)) +
  scale_color_manual(values = cols_problem,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "") +
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
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))



ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "fig1_SI_maps_problems.png"), 
       width = 8, height = 6)


## Pie chart

grid_cell_area <- unique(evap_trend_stats[, .(lon, lat)]) %>% grid_area() # m2
evap_trend_stats <- grid_cell_area[evap_trend_stats, on = .(lon, lat)]
total_area <- evap_trend_stats[, sum(area)]
problem_area_stats <- evap_trend_stats[, .(area_fraction = sum(area)/total_area), .(problem)]

problem_area_stats <- problem_area_stats[,problem_area_stats[order(problem, decreasing = T)]]

cols_problem <- c("#330000","orange", "darkred", "lightblue", "darkblue")


ggplot(problem_area_stats , aes(x = "", y = area_fraction*100))+
  geom_bar(aes(fill = problem), stat = "identity") +
  xlab('')  +
  ylab('Area fraction [%]')  +
  labs(fill = '')  +
  scale_fill_manual(values = cols_problem)+
  theme_light() +
  theme(strip.text.x = element_text(color = "black"),
        strip.background = element_rect(color="black", fill="white", linetype="solid"),
        axis.text = element_blank(),
        legend.position = "bottom",
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16)
  )+
  geom_text(aes(x = 1.8, label = round(area_fraction*100, digits = 1)),
            position = position_stack(vjust = 0.5), show.legend = FALSE) +
  coord_polar(theta = "y", start = 0)+
  guides(fill = guide_legend(nrow = 3, byrow=TRUE))

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "fig1_SI_PIE_problems.png"), 
       width = 8, height = 6)
