# Test for normality on each grid cell

source("source/uncertainty_prec.R")

install.packages(setdiff("rnaturalearth", rownames(installed.packages())))
library(rnaturalearth)
library(sf)
library(stars)

## Data
prec_month <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_data_month.rds"))

prec_years <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_data_years.rds"))

## Analysis
### Month
test_month <- prec_month[, .(sw_p = shapiro.test(prec)$p.value),
                         .(lon , lat, date)]

test_month[, sw := ifelse(sw_p > 0.05, 1, 0)]

test_month <- test_month[, .(sw = sum(sw)), .(lon, lat)]

gauss_month <- test_month[, .(gauss = ifelse(sw >= 183, 1, 0)), .(lon, lat)]

### Year
test_year <- prec_years[, .(sw_p = shapiro.test(prec)$p.value),
                        .(lon , lat, date)]

test_year[, sw := ifelse(sw_p > 0.05, 1, 0)]

test_year <- test_year[, .(sw = sum(sw)), .(lon, lat)]

gauss_year <- test_year[, .(gauss = ifelse(sw >= 16, 1, 0)), .(lon, lat)]

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
test_year[sw > 17, level := 1
          ][sw > 15 & sw <= 17, level := 2
            ][sw > 9 & sw <= 15, level := 3
              ][sw <= 9, level := 4]

test_month[sw > 216, level := 1
           ][sw > 182 & sw <= 216, level := 2
             ][sw > 114 & sw <= 182, level := 3
               ][sw <= 114, level := 4]

to_plot_sf_month <- test_month[, .(lon, lat, level)
                               ][, value := as.numeric(level)]

to_plot_sf_years <- test_year[, .(lon, lat, level)
                              ][, value := as.numeric(level)]

to_plot_sf_month <- to_plot_sf_month[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_sf_years <- to_plot_sf_years[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

col_vals <- c("#081d58", "#1d91c0", "#c7e9b4", "#ffffd9")
col_labs <- c("Very High", "High", "Moderate", "Low")

### Map
p00 <- ggplot(to_plot_sf_years) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = as.factor(value), fill = as.factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 2) +
  scale_fill_manual(values = col_vals, labels = col_labs) +
  scale_color_manual(values = col_vals, guide = "none") +
  labs(x = NULL, y = NULL, fill = "Normality\nLevel") +
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
              "normality_map_yrs.png"), width = 16, height = 9)

p01 <- ggplot(to_plot_sf_month) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = as.factor(value), fill = as.factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 2) +
  scale_fill_manual(values = col_vals, labels = col_labs) +
  scale_color_manual(values = col_vals, guide = "none") +
  labs(x = NULL, y = NULL, fill = "Normality\nLevel") +
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

ggsave(plot = p01, paste0(PATH_SAVE_UNCERTAINTY_PREC_FIGURES,
                          "normality_map_mon.png"), width = 16, height = 9)

## Save
save(test_month, gauss_month, test_year, gauss_year,
     file = paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_gaussian.rda"))
