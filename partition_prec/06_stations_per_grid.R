# Plot number of stations per grid cell

source('source/partition_prec.R')
source('source/geo_functions.R')
source('source/graphics.R')

# Data
prec_grid <- raster(paste0(PATH_SAVE_PARTITION_PREC,
                           "prec_station_grid.nc")) %>%
  as.data.frame(xy = TRUE, long = TRUE, na.rm = TRUE) %>%
  setnames(., c("x", "y", "value"), c("lon", "lat", "no_stations"))

#Figure

p00 <- ggplot(prec_grid) +
  borders() +
  geom_point(aes(x = lon, y = lat, color = no_stations), size = 0.1) +
  scale_color_viridis_c(option = "H") +
  theme_light() +
  labs(x = "Lon", y = "Lat", color = "No.\nStations") +
  coord_cartesian(xlim = c(-180, 180), ylim = c(-90,90), expand = FALSE) +
  theme_opts
y_labs <- ggplot_build(p00)$layout$panel_params[[1]]$y$get_labels()
x_labs <- ggplot_build(p00)$layout$panel_params[[1]]$x$get_labels()
p01 <- p00 + scale_x_continuous(labels = paste0(x_labs, "\u00b0")) +
  scale_y_continuous(labels = paste0(y_labs, "\u00b0"))

p00 <- ggplot(prec_grid) +
  borders() +
  geom_tile(aes(x = lon, y = lat, fill = no_stations)) +
  scale_fill_viridis_c(option = "H") +
  theme_light() +
  labs(x = "Lon", y = "Lat", fill = "No.\nStations") +
  coord_cartesian(xlim = c(-180, 180), ylim = c(-90,90), expand = FALSE) +
  theme_opts
y_labs <- ggplot_build(p00)$layout$panel_params[[1]]$y$get_labels()
x_labs <- ggplot_build(p00)$layout$panel_params[[1]]$x$get_labels()
p01 <- p00 + scale_x_continuous(labels = paste0(x_labs, "\u00b0")) +
  scale_y_continuous(labels = paste0(y_labs, "\u00b0"))
