# Plot number of stations per grid cell

source('source/partition_prec.R')
source('source/geo_functions.R')
source('source/graphics.R')

# Data
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))
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
  coord_sf(expand = FALSE, default_crs = sf::st_crs(54024)) +
  theme_opts +
  scale_x_continuous(breaks = seq(-150, 150, 30),
                     labels = paste0(seq(-150, 150, 30), "\u00b0")) +
  scale_y_continuous(breaks = seq(-60, 60, 30),
                     labels = paste0(seq(-60, 60, 30), "\u00b0"))

p00 <- ggplot(prec_grid) +
  borders() +
  geom_tile(aes(x = lon, y = lat, fill = no_stations)) +
  scale_fill_viridis_c(option = "H") +
  theme_light() +
  labs(x = "Lon", y = "Lat", fill = "No.\nStations") +
  coord_sf(expand = FALSE, default_crs = sf::st_crs(4326)) +
  theme_opts +
  scale_x_continuous(breaks = seq(-150, 150, 30),
                     labels = paste0(seq(-150, 150, 30), "\u00b0")) +
  scale_y_continuous(breaks = seq(-60, 60, 30),
                     labels = paste0(seq(-60, 60, 30), "\u00b0"))
