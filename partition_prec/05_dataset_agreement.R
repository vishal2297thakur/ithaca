# Plot global map of dataset agreement classses 
source('source/partition_prec.R')
source('source/geo_functions.R')
source('source/graphics.R')

library(rnaturalearth)

# Data
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))
levels(prec_mask$rel_dataset_agreement) <- c("High", "Above average", "Average",
                                             "Below average", "Low")

station_grid <- read_stars(paste0(PATH_SAVE_PARTITION_PREC_SPATIAL,
                               "prec_station_grid.nc")) %>% st_as_sf()
colnames(station_grid)[1] <- "value"
st_crs(station_grid) <- "+proj=longlat +datum=WGS84 +no_defs"

#World and Land borders
earth_box <- readRDS(paste0(PATH_SAVE_PARTITION_PREC_SPATIAL,
                            "earth_box.rds")) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")
world_sf <- ne_countries(returnclass = "sf")

#Labels
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

# Figures
fig_stations <- ggplot(station_grid) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(color = "dark red") +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 3) +
  scale_color_viridis_c(option = "H") +
  labs(x = NULL, y = NULL) +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  theme_bw() +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray40", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray40", size = 4) +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 24), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 24))

ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES,
              "supplement/station_map.png"), width = 12, height = 8)

to_plot_sf <- prec_mask[, .(lon, lat, rel_dataset_agreement)
][, value := as.numeric(rel_dataset_agreement)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_dataset_agreement <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = factor(value), fill = factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 3) +
  scale_fill_manual(values = colset_RdBu_5,
                    labels = levels(to_plot_sf$rel_dataset_agreement)) +
  scale_color_manual(values = colset_RdBu_5,
                     labels = levels(to_plot_sf$rel_dataset_agreement),
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Dataset\nAgreement") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray40", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray40", size = 4) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES,
              "dataset_agreement_map.png"), width = 12, height = 8)

to_plot_sf <- prec_mask[, .(lon, lat, abs_dataset_agreement)
][, value := as.numeric(abs_dataset_agreement)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_dataset_agreement <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = factor(value), fill = factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 3) +
  scale_fill_manual(values = c('dark red', colset_RdBu_5, 'dark blue'),
                    labels = levels(to_plot_sf$rel_dataset_agreement)) +
  scale_color_manual(values = c('dark red', colset_RdBu_5, 'dark blue'),
                     labels = levels(to_plot_sf$rel_dataset_agreement),
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Dataset\nAgreement") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray40", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray40", size = 4) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES,
              "abs_dataset_agreement_map.png"), width = 12, height = 8)
