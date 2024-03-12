# Test for normality on each grid cell

source("source/uncertainty_prec.R")

install.packages(setdiff("rnaturalearth", rownames(installed.packages())))
library(rnaturalearth)
library(sf)
library(stars)

## Data
prec_years <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_data_years.rds"))

prec_month <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_data_month.rds"))

prec_data <- copy(prec_years)

## Analysis
prec_data <- prec_data[, .(prec = mean(prec, na.rm = TRUE)), .(lon, lat, dataset)]
prec_data <- prec_data[, n_datasets := .N, .(lon, lat)][, .(lon, lat, n_datasets)]
prec_data <- unique(prec_data)

lonlat <- prec_data[n_datasets == MIN_N_DATASETS]

prec_years <- prec_years[lonlat[, .(lon, lat)], on = .(lon, lat)]
prec_month <- prec_month[lonlat[, .(lon, lat)], on = .(lon, lat)]

## Save data
saveRDS(prec_month, paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_data_month.rds"))
saveRDS(prec_years, paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_data_years.rds"))

## Figure
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
### Map data
to_plot_sf <- prec_data %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

col_vals <- rev(c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
                           "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6",
                           "#6a3d9a"))

### Map
p00 <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = as.factor(n_datasets), fill = as.factor(n_datasets))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 2) +
  labs(x = NULL, y = NULL, fill = "Number of\nData Sets") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  scale_fill_manual(values = col_vals) +
  scale_color_manual(values = col_vals, guide = "none") +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray40", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray40", size = 4) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

### Save
ggsave(plot = p00, paste0(PATH_SAVE_UNCERTAINTY_PREC_FIGURES,
                          "n_datasets.png"), width = 16, height = 9)
