# Plots global maps of dataset agreement classes ----
source('source/partition_evap.R')
source('source/geo_functions.R')
source('source/graphics.R')

library(rnaturalearth)

## Dry Data ----
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_stats_ssi_era5-land_dry.rds"))
levels(evap_mask$rel_dataset_agreement) <- c("High", "Above average", "Average",
                                             "Below average", "Low")
levels(evap_mask$evap_quant_dataset_agreement) <- c("High", "Above average", "Average",
                                                    "Below average", "Low")



## World and Land borders ----
earth_box <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP_SPATIAL,
                            "earth_box.rds")) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")
world_sf <- ne_countries(returnclass = "sf")

## Labels
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

## Figures ----

### Relative dataset agreement ----
to_plot_sf <- evap_mask[, .(lon, lat, rel_dataset_agreement)
][, value := as.numeric(rel_dataset_agreement)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_rel_dataset_agreement <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = factor(value), fill = factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = rev(colset_RdBu_5),
                    labels = levels(evap_mask$rel_dataset_agreement)) +
  scale_color_manual(values = rev(colset_RdBu_5),
                     labels = levels(evap_mask$rel_dataset_agreement),
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

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
              "rel_dataset_agreement_map_ssi_era5-land_dry.png"), width = 12, height = 8)

### Relative dataset agreement conditioned by evaporation rate ----
to_plot_sf <- evap_mask[, .(lon, lat, evap_quant_dataset_agreement)
][, value := as.numeric(evap_quant_dataset_agreement )]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_evap_quant_dataset_agreement <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = factor(value), fill = factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = rev(colset_RdBu_5), 
                    labels = levels(evap_mask$evap_quant_dataset_agreement)) +
  scale_color_manual(values = rev(colset_RdBu_5),
                     labels = levels(evap_mask$evap_quant_dataset_agreement),
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

gg_agreement <- ggarrange(fig_rel_dataset_agreement, fig_evap_quant_dataset_agreement,
                          labels = c('a', 'b'), common.legend = TRUE,
                          legend = 'right', align = 'hv',
                          nrow = 2, ncol = 1)

jpeg(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "dataset_agreement_maps_ssi_era5-land_dry.png"), 
     width = 12, height = 12, res = 300, units = 'in')
gg_agreement
dev.off()

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
              "evap_quant_dataset_agreement_map_ssi_era5-land_dry.png"), width = 12, height = 8)

## Wet Data ----

evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_stats_ssi_era5-land_wet.rds"))
levels(evap_mask$rel_dataset_agreement) <- c("High", "Above average", "Average",
                                             "Below average", "Low")
levels(evap_mask$evap_quant_dataset_agreement) <- c("High", "Above average", "Average",
                                                    "Below average", "Low")


### Relative dataset agreement ----
to_plot_sf <- evap_mask[, .(lon, lat, rel_dataset_agreement)
][, value := as.numeric(rel_dataset_agreement)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_rel_dataset_agreement <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = factor(value), fill = factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = rev(colset_RdBu_5),
                    labels = levels(evap_mask$rel_dataset_agreement)) +
  scale_color_manual(values = rev(colset_RdBu_5),
                     labels = levels(evap_mask$rel_dataset_agreement),
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

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
              "rel_dataset_agreement_map_ssi_era5-land_wet.png"), width = 12, height = 8)

### Relative dataset agreement conditioned by evaporation rate ----
to_plot_sf <- evap_mask[, .(lon, lat, evap_quant_dataset_agreement)
][, value := as.numeric(evap_quant_dataset_agreement )]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_evap_quant_dataset_agreement <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = factor(value), fill = factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = rev(colset_RdBu_5), 
                    labels = levels(evap_mask$evap_quant_dataset_agreement)) +
  scale_color_manual(values = rev(colset_RdBu_5),
                     labels = levels(evap_mask$evap_quant_dataset_agreement),
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

gg_agreement <- ggarrange(fig_rel_dataset_agreement, fig_evap_quant_dataset_agreement,
                          labels = c('a', 'b'), common.legend = TRUE,
                          legend = 'right', align = 'hv',
                          nrow = 2, ncol = 1)

jpeg(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "dataset_agreement_maps_ssi_era5-land_wet.png"), 
     width = 12, height = 12, res = 300, units = 'in')
gg_agreement
dev.off()

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
              "evap_quant_dataset_agreement_map_ssi_era5-land_wet.png"), width = 12, height = 8)


## Normal Data ----

evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_stats_ssi_era5-land_normal.rds"))
levels(evap_mask$rel_dataset_agreement) <- c("High", "Above average", "Average",
                                             "Below average", "Low")
levels(evap_mask$evap_quant_dataset_agreement) <- c("High", "Above average", "Average",
                                                    "Below average", "Low")


### Relative dataset agreement ----
to_plot_sf <- evap_mask[, .(lon, lat, rel_dataset_agreement)
][, value := as.numeric(rel_dataset_agreement)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_rel_dataset_agreement <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = factor(value), fill = factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = rev(colset_RdBu_5),
                    labels = levels(evap_mask$rel_dataset_agreement)) +
  scale_color_manual(values = rev(colset_RdBu_5),
                     labels = levels(evap_mask$rel_dataset_agreement),
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

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
              "rel_dataset_agreement_map_ssi_era5-land_normal.png"), width = 12, height = 8)

### Relative dataset agreement conditioned by evaporation rate ----
to_plot_sf <- evap_mask[, .(lon, lat, evap_quant_dataset_agreement)
][, value := as.numeric(evap_quant_dataset_agreement )]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_evap_quant_dataset_agreement <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = factor(value), fill = factor(value))) +
  geom_sf(data = earth_box, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_manual(values = rev(colset_RdBu_5), 
                    labels = levels(evap_mask$evap_quant_dataset_agreement)) +
  scale_color_manual(values = rev(colset_RdBu_5),
                     labels = levels(evap_mask$evap_quant_dataset_agreement),
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

gg_agreement <- ggarrange(fig_rel_dataset_agreement, fig_evap_quant_dataset_agreement,
                          labels = c('a', 'b'), common.legend = TRUE,
                          legend = 'right', align = 'hv',
                          nrow = 2, ncol = 1)

jpeg(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "dataset_agreement_maps_ssi_era5-land_normal.png"), 
     width = 12, height = 12, res = 300, units = 'in')
gg_agreement
dev.off()

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
              "evap_quant_dataset_agreement_map_ssi_era5-land_normal.png"), width = 12, height = 8)
