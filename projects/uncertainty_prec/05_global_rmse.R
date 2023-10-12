# Calculate RMSE to median time series

source("source/uncertainty_prec.R")

install.packages(setdiff(c("DescTools", "Metrics", "rnaturalearth"),
                         rownames(installed.packages())))

library(DescTools, include.only = "Mode")
library(Metrics)
library(rnaturalearth)
library(sf)
library(stars)

## Data
prec_month <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_data_month.rds"))

prec_years <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_data_years.rds"))

## Analysis
prec_month[, median_prec := median(prec, na.rm = TRUE), .(lon, lat, date)]
prec_month <- prec_month[, .(mse_prec = mean((median_prec - prec)^2,
                                             na.rm = TRUE),
                             r_prec = cor(prec, median_prec),
                             bias_prec = mean(prec, na.rm = TRUE) -
                               mean(median_prec, na.rm = TRUE),
                             var_prec = var(prec, na.rm = TRUE),
                             median_var = var(median_prec, na.rm = TRUE)),
                         .(lon, lat, dataset)]

prec_month <- prec_month[, .(t_prec = ((1 + r_prec)/2)*(1 - (mse_prec/((bias_prec^2) + (var_prec^2) + (median_var^2))))),
                         .(lon, lat, dataset)]


prec_years[, median_prec := median(prec, na.rm = TRUE), .(lon, lat, date)]
prec_years <- prec_years[, .(mse_prec = mean((median_prec - prec)^2,
                                             na.rm = TRUE),
                             r_prec = cor(prec, median_prec),
                             bias_prec = mean(prec, na.rm = TRUE) -
                               mean(median_prec, na.rm = TRUE),
                             var_prec = var(prec, na.rm = TRUE),
                             median_var = var(median_prec, na.rm = TRUE)),
                         .(lon, lat, dataset)]

prec_years <- prec_years[, .(t_prec = ((1 + r_prec)/2)*(1 - (mse_prec/((bias_prec^2) + (var_prec^2) + (median_var^2))))),
                         .(lon, lat, dataset)]

## Save
saveRDS(prec_month, paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_month.rds"))
saveRDS(prec_years, paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_years.rds"))

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
to_plot_month <- prec_month[ , .SD[which.max(t_prec)], by = .(lon, lat)]

to_plot_years <- prec_years[ , .SD[which.max(t_prec)], by = .(lon, lat)]

to_plot_month[dataset %in% PREC_DATASETS_OBS, dataset_type := 1
              ][dataset %in% PREC_DATASETS_REANAL, dataset_type := 2
                ][dataset %in% PREC_DATASETS_REMOTE, dataset_type := 3
                  ][dataset %in% PREC_DATASETS_HYDROL, dataset_type := 4]

to_plot_years[dataset %in% PREC_DATASETS_OBS, dataset_type := 1
              ][dataset %in% PREC_DATASETS_REANAL, dataset_type := 2
                ][dataset %in% PREC_DATASETS_REMOTE, dataset_type := 3
                  ][dataset %in% PREC_DATASETS_HYDROL, dataset_type := 4]

to_plot_month <- to_plot_month[, .(lon, lat, dataset_type)
                               ][, value := as.numeric(dataset_type)]

to_plot_years <- to_plot_years[, .(lon, lat, dataset_type)
                               ][, value := as.numeric(dataset_type)]

to_plot_month <- to_plot_month[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_years <- to_plot_years[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

col_vals <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")

col_labs <- c("Ground Stations", "Reanalysis", "Remote Sensing",
              "Hydrological Models")

### Map
p00 <- ggplot(to_plot_month) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = as.factor(value), fill = as.factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 2) +
  scale_fill_manual(values = col_vals, labels = col_labs) +
  scale_color_manual(values = col_vals, guide = "none") +
  labs(x = NULL, y = NULL, fill = "Dataset\nType") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
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

ggsave(plot = p00, paste0(PATH_SAVE_UNCERTAINTY_PREC_FIGURES,
                          "best_data_type_mon_t_metric.png"), width = 16, height = 9)

p01 <- ggplot(to_plot_years) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = as.factor(value), fill = as.factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 2) +
  scale_fill_manual(values = col_vals, labels = col_labs) +
  scale_color_manual(values = col_vals, guide = "none") +
  labs(x = NULL, y = NULL, fill = "Dataset\nType") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
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

ggsave(plot = p01,
       paste0(PATH_SAVE_UNCERTAINTY_PREC_FIGURES,
              "best_data_type_yrs_t_metric.png"), width = 16, height = 9)
