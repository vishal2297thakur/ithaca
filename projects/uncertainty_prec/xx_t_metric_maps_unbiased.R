# Maps
source("source/uncertainty_prec.R")

install.packages(setdiff("rnaturalearth", rownames(installed.packages())))
library(rnaturalearth)
library(sf)
library(stars)
library(viridis)

## Data
prec_month <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC,
                             "t_metric_unbiased.rds"))

## Analysis
t_max <- prec_month[, .SD[which.max(t_prec)], by = .(lon, lat)]

t_min <- prec_month[, .SD[which.min(t_prec)], by = .(lon, lat)]

t_range <- merge(t_max, t_min, by = c("lon", "lat"))
t_range <- t_range[, .(t_prec = t_prec.x - t_prec.y), .(lon, lat)]

t_avg <- prec_month[, .(t_prec = mean(t_prec, na.rm = TRUE)), .(lon, lat)]
### Borders and labels
earth_box <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC_SPATIAL,
                            "earth_box.rds")) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")

world_sf <- ne_countries(returnclass = "sf")

labs_y <- data.frame(lon = -170, lat = c(55, 25, -5, -35, -65))

labs_y_labels <- seq(60, -60, -30)

labs_y$label <- ifelse(labs_y_labels == 0, "°",
                       ifelse(labs_y_labels > 0, "°N", "°S"))

labs_y$label <- paste0(abs(labs_y_labels), labs_y$label)

labs_y <- st_as_sf(labs_y, coords = c("lon", "lat"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")

labs_x <- data.frame(lon = seq(120, -120, -60), lat = -82)

labs_x$label <- ifelse(labs_x$lon == 0, "°", ifelse(labs_x$lon > 0, "°E", "°W"))

labs_x$label <- paste0(abs(labs_x$lon), labs_x$label)

labs_x <- st_as_sf(labs_x, coords = c("lon", "lat"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")

###
to_plot_avg <- t_avg[, .(lon, lat, t_prec)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_max <- t_max[, .(lon, lat, t_prec)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_range <- t_range[, .(lon, lat, t_prec)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

p01 <- ggplot(to_plot_avg) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = t_prec, fill = t_prec)) +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_fill_viridis(limits = c(0,1)) +
  scale_colour_viridis(limits = c(0,1), guide = "none") +
  labs(x = NULL, y = NULL, fill = NULL) +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray23", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray23", size = 4) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray69", linetype = "dashed"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

p02 <- ggplot(to_plot_range) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = t_prec, fill = t_prec)) +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_fill_viridis(limits = c(0,1)) +
  scale_colour_viridis(limits = c(0,1), guide = "none") +
  labs(x = NULL, y = NULL, fill = NULL) +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray23", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray23", size = 4) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray69", linetype = "dashed"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

###
p00 <- ggarrange(p01, p02, ncol = 2, common.legend = TRUE,
                 legend = "right", labels = c("(a)", "(b)"))

ggsave(plot = p00,
       paste0(PATH_SAVE_UNCERTAINTY_PREC_FIGURES,
              "t-metric_maps_unbiased.png"), width = 4.5*GOLDEN_RATIO*2,
       height = 4.5)
