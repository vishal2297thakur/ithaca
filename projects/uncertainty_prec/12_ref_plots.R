# Time Series
source("source/uncertainty_prec.R")

install.packages(setdiff("rnaturalearth", rownames(installed.packages())))
library(rnaturalearth)
library(sf)
library(stars)

## Data
prec_map <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_data_roi.rds"))

PREC_REPS <- c("cmap", "cpc", "cru-ts-v4-07", "em-earth", "era5-land", "fldas",
               "gpcp-v3-2", "jra55", "ncep-doe", "precl")

prec_map <- prec_map[dataset %in% PREC_REPS]
prec_map <- prec_map[, .(median_prec = median(prec, na.rm = TRUE)),
                     .(lon, lat, date)]

prec_map[, YEAR := year(date)]

prec_map <- prec_map[, .(value = sum(median_prec, na.rm = TRUE)),
                     .(lon, lat, YEAR)]

prec_map <- prec_map[, .(prec = mean(value, na.rm = TRUE)), .(lon, lat)]

MAP_MAX <- quantile(prec_map$prec, 0.9995)

prec_map[prec >= MAP_MAX, prec := MAP_MAX + 1]

prec_data <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_time_series.rds"))

prec_data_median <- prec_data[dataset %in% PREC_REPS,
                              .(value = median(value, na.rm = TRUE),
                                dataset = "reference"), .(date)]

prec_data[, `:=`(upper = max(value), lower = min(value)), .(date)]

to_plot_data <- prec_map[, .(lon, lat, prec)] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

### Borders and labels
earth_box <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC_SPATIAL,
                            "earth_box.rds")) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")

world_sf <- ne_countries(returnclass = "sf")

labs_y <- data.frame(lon = -162, lat = c(55, 25, -5, -35, -65))
labs_y_labels <- seq(60, -60, -30)
labs_y$label <- ifelse(labs_y_labels == 0, "°",
                       ifelse(labs_y_labels > 0, "°N", "°S"))
labs_y$label <- paste0(abs(labs_y_labels), labs_y$label)
labs_y <- st_as_sf(labs_y, coords = c("lon", "lat"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")

labs_x <- data.frame(lon = seq(120, -120, -60), lat = -80)
labs_x$label <- ifelse(labs_x$lon == 0, "°", ifelse(labs_x$lon > 0, "°E", "°W"))
labs_x$label <- paste0(abs(labs_x$lon), labs_x$label)
labs_x <- st_as_sf(labs_x, coords = c("lon", "lat"),
                   crs = "+proj=longlat +datum=WGS84 +no_defs")


p01 <- ggplot(prec_data_median) +
  geom_ribbon(data = prec_data, aes(ymax = upper, ymin = lower, x = date),
              fill = "gray69") +
  geom_line(aes(x = date, y = value), color = "#377eb8", linewidth = 1) +
  labs(x = "Year", y = "Precipitation [mm/month]") +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid = element_blank(), 
        axis.line = element_line(colour = "gray23", linewidth = 1),
        plot.title = element_text(size = 20), 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        axis.ticks.length = unit(0.25, "cm"))

p02 <- ggplot(to_plot_data) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = prec, fill = prec)) +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1,
                       guide = guide_colorbar(frame.colour = "gray23",
                                              ticks.colour = "gray23")) +
  scale_color_distiller(palette = "YlGnBu", direction = 1, guide = "none") +
  labs(x = NULL, y = NULL, fill = "[mm/year]",
       title = "Average Annual Precipitation 2001 - 2019") +
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
        plot.title = element_text(size = 22, hjust = 0.5),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

p00 <- ggarrange(p01, p02, ncol = 1, nrow = 2, labels = c("a)", "b)"))

ggsave(plot = p00,
       paste0(PATH_SAVE_UNCERTAINTY_PREC_FIGURES, "reference_data.pdf"),
       width = 5*GOLDEN_RATIO, height = 5*2)
