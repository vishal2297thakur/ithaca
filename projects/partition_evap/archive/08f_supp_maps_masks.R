# Supplementary figure: Masks ----
source('source/partition_evap.R')
source('source/geo_functions.R')
source('source/graphics.R')

library(rnaturalearth)
library(dplyr)

# Data ----
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
levels(evap_mask$land_cover_short_class) <- c("Barren", "Croplands", "Forests", "Grasslands", "Other", "Savannas", 
                                            "Shrublands", "Snow/Ice", "Water" )
ipcc_sf <- read_sf("~/shared/data/geodata/ipcc_v4/IPCC-WGI-reference-regions-v4.shp")
IPCC_list <- evap_mask[, unique(as.character(IPCC_ref_region))]
IPCC_list <- IPCC_list[!is.na(IPCC_list)]

ipcc_sf_terr <- ipcc_sf[ipcc_sf$Acronym %in% IPCC_list,]

# World and Land borders -----
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

# Figures ----
## land use ----
to_plot_sf <- evap_mask[, .(lon, lat, land_cover_short_class)
][, value := as.numeric(land_cover_short_class)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_sf <- to_plot_sf %>% mutate(land_cover_short_class = 
                                      case_when(to_plot_sf$value == 1 ~ "Barren", 
                                                to_plot_sf$value == 2 ~ "Croplands", 
                                                to_plot_sf$value == 3 ~ "Forests", 
                                                to_plot_sf$value == 4 ~ "Grasslands", 
                                                to_plot_sf$value == 5 ~ "Other", 
                                                to_plot_sf$value == 6 ~ "Savannas", 
                                                to_plot_sf$value == 7 ~ "Shrublands", 
                                                to_plot_sf$value == 8 ~ "Snow/Ice", 
                                                to_plot_sf$value == 9 ~ "Water"
                                                ))

to_plot_sf$land_cover_short_class <- factor(to_plot_sf$land_cover_short_class, 
                                           levels = c("Barren", "Croplands", "Forests", 
                                                      "Grasslands", "Other", "Savannas", 
                                                      "Shrublands", "Snow/Ice", "Water"), 
                                           ordered = TRUE)

fig_land_cover_short_class <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = land_cover_short_class, fill = land_cover_short_class)) +
  geom_sf(data = earth_box, fill = NA, color = "gray", lwd = 0.1) +
  scale_fill_manual(values = colset_land_cover_short) + 
  scale_color_manual(values = colset_land_cover_short,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Land cover") +
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
              "supplement/land_cover_map.png"), width = 12, height = 8)


## biome ----

evap_mask[grepl("Tundra", biome_class) == TRUE, biome_short_class := "Tundra"]
evap_mask[grepl("Boreal Forests", biome_class) == TRUE, biome_short_class := "B. Forests"]
evap_mask[grepl("Dry Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Dry BL Forests"]
evap_mask[grepl("Moist Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Moist BL Forests"]
evap_mask[grepl("Subtropical Coniferous Forests", biome_class) == TRUE, biome_short_class := "T/S Coni. Forests"]
evap_mask[grepl("Temperate Conifer Forests", biome_class) == TRUE, biome_short_class := "T. Coni. Forests"]
evap_mask[grepl("Temperate Broadleaf & Mixed Forests", biome_class) == TRUE, biome_short_class := "T. BL Forests"]
evap_mask[grepl("Temperate Grasslands", biome_class) == TRUE, biome_short_class := "T. Grasslands"]
evap_mask[grepl("Subtropical Grasslands", biome_class) == TRUE, biome_short_class := "T/S Grasslands"]
evap_mask[grepl("Montane Grasslands", biome_class) == TRUE, biome_short_class := "M. Grasslands"]
evap_mask[grepl("Flooded", biome_class) == TRUE, biome_short_class := "Flooded"]
evap_mask[grepl("Mangroves", biome_class) == TRUE, biome_short_class := "Mangroves"]
evap_mask[grepl("Deserts", biome_class) == TRUE, biome_short_class := "Deserts"]
evap_mask[grepl("Mediterranean", biome_class) == TRUE, biome_short_class := "Mediterranean"]
evap_mask[grepl("N/A", biome_class) == TRUE, biome_short_class := NA]
evap_mask[, biome_short_class := factor(biome_short_class)]

to_plot_sf <- evap_mask[, .(lon, lat, biome_short_class)
][, value := as.numeric(biome_short_class)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_sf <- to_plot_sf %>% mutate(biome_short_class = 
                                      case_when(to_plot_sf$value == 1 ~ "B. Forests", 
                                                to_plot_sf$value == 2 ~ "Deserts", 
                                                to_plot_sf$value == 3 ~ "Flooded", 
                                                to_plot_sf$value == 4 ~ "M. Grasslands", 
                                                to_plot_sf$value == 5 ~ "Mediterranean", 
                                                to_plot_sf$value == 6 ~ "T. Grasslands", 
                                                to_plot_sf$value == 7 ~ "T/S Grasslands", 
                                                to_plot_sf$value == 8 ~ "Tundra",
                                                to_plot_sf$value == 9 ~ "T/S Dry BL Forests", 
                                                to_plot_sf$value == 10 ~ "T/S Moist BL Forests", 
                                                to_plot_sf$value == 11 ~ "T/S Coni. Forests",
                                                to_plot_sf$value == 12 ~ "T. Coni. Forests",
                                                to_plot_sf$value == 13 ~ "T. BL Forests",
                                                to_plot_sf$value == 14 ~ "Mangroves"
                                                ))

to_plot_sf$biome_short_class <- factor(to_plot_sf$biome_short_class, 
                                           levels = c("B. Forests", "Deserts", "Flooded", "Mangroves",
                                                      "M. Grasslands", "Mediterranean", 
                                                      "T. Coni. Forests", "T. BL Forests", 
                                                      "T. Grasslands", 
                                                      "T/S Coni. Forests", "T/S Dry BL Forests", "T/S Moist BL Forests",
                                                      "T/S Grasslands", "Tundra"), ordered = TRUE)
fig_biome_short_class <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = biome_short_class, fill = biome_short_class)) +
  geom_sf(data = earth_box, fill = NA, color = "gray", lwd = 0.1) +
  scale_fill_manual(values = colset_biome) + 
  scale_color_manual(values = colset_biome,
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Biome") +
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
              "supplement/biome_map.png"), width = 12, height = 8)

## elevation ----
levels(evap_mask$elev_class) <- c("0-100", "100-400", "400-800", "800-1500", "1500-3000", "3000+")


to_plot_sf <- evap_mask[, .(lon, lat, elev_class)
][, value := as.numeric(elev_class)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_sf <- to_plot_sf %>% mutate(elev_class = 
                                      case_when(to_plot_sf$value == 1 ~ "0-100", 
                                                to_plot_sf$value == 2 ~ "100-400", 
                                                to_plot_sf$value == 3 ~ "400-800", 
                                                to_plot_sf$value == 4 ~ "800-1500", 
                                                to_plot_sf$value == 5 ~ "1500-3000", 
                                                to_plot_sf$value == 6 ~ "3000+"))

to_plot_sf$elev_class <- factor(to_plot_sf$elev_class, 
                                       levels = c("0-100", "100-400", "400-800", 
                                                  "800-1500", "1500-3000", "3000+"), ordered = TRUE)
fig_elev_class <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = elev_class, fill = elev_class)) +
  geom_sf(data = earth_box, fill = NA, color = "gray", lwd = 0.1) +
  scale_fill_manual(values = rev(c(colset_elev_mono))) + 
  scale_color_manual(values = rev(c(colset_elev_mono)), 
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Elevation") +
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
              "supplement/elev_map.png"), width = 12, height = 8)


## evap_quantile ----
levels(evap_mask$evap_quant) <- c("0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", 
                                  "0.4-0.5", "0.5-0.6", "0.6-0.7", "0.7-0.8", 
                                  "0.8-0.9", "0.9-1")


to_plot_sf <- evap_mask[, .(lon, lat, evap_quant)
][, value := as.numeric(evap_quant)]
to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_sf <- to_plot_sf %>% mutate(evap_quant = 
                                      case_when(to_plot_sf$value == 1 ~ "0-0.1", 
                                                to_plot_sf$value == 2 ~ "0.1-0.2", 
                                                to_plot_sf$value == 3 ~ "0.2-0.3", 
                                                to_plot_sf$value == 4 ~ "0.3-0.4", 
                                                to_plot_sf$value == 5 ~ "0.4-0.5", 
                                                to_plot_sf$value == 6 ~ "0.5-0.6", 
                                                to_plot_sf$value == 7 ~ "0.6-0.7", 
                                                to_plot_sf$value == 8 ~ "0.7-0.8", 
                                                to_plot_sf$value == 9 ~ "0.8-0.9", 
                                                to_plot_sf$value == 10 ~ "0.9-1"))

to_plot_sf$evap_quant <- factor(to_plot_sf$evap_quant, 
                                levels = c("0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", 
                                           "0.4-0.5", "0.5-0.6", "0.6-0.7", "0.7-0.8", 
                                           "0.8-0.9", "0.9-1"), ordered = TRUE)
fig_evap_quant_class <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(aes(color = evap_quant, fill = evap_quant)) +
  geom_sf(data = earth_box, fill = NA, color = "gray", lwd = 0.1) +
  scale_fill_manual(values = c(colset_prec_quant)) + 
  scale_color_manual(values = c(colset_prec_quant), 
                     guide = "none") + 
  labs(x = NULL, y = NULL, fill = "Evaporation\nquantile") +
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
              "supplement/evap_quant_map.png"), width = 12, height = 8)

## IPCC ----
evap_mask[, IPCC_ref_region := as.factor(IPCC_ref_region)]

to_plot_sf <- evap_mask[, .(lon, lat, IPCC_ref_region)
][, value := as.numeric(IPCC_ref_region)]

to_plot_sf <- to_plot_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

fig_ipcc <- ggplot(to_plot_sf) +
  geom_sf(data = world_sf, fill = "light gray", color = "light gray") +
  geom_sf(data = earth_box, fill = NA, color = "gray", lwd = 0.1) +
  geom_sf(data = ipcc_sf_terr , fill = "gold", color = "gray23", alpha = 0.4) +
  geom_sf_text(data = ipcc_sf_terr, aes(label = Acronym), size = 3,
               fontface = "bold") +
  labs(x = NULL, y = NULL, fill = "") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_y_continuous(breaks = seq(-60, 60, 30)) +
  geom_sf_text(data = labs_y, aes(label = label), color = "gray40", size = 4) +
  geom_sf_text(data = labs_x, aes(label = label), color = "gray40", size = 4) +
  theme_bw() +
  ggtitle("IPCC reference regions v4")+
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.border = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.text = element_blank(), 
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 16))

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
              "supplement/ipcc_map.png"), width = 9, height = 6)

## Composite figure ----
ggarrange(fig_land_cover_short_class, fig_biome_short_class, fig_elev_class, fig_evap_quant_class, align = "hv",
          labels = c("a", "b", "c", "d"))

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
              "supplement/masks_maps.png"), width = 15, height = 8)

ggarrange(fig_evap_quant_class, fig_ipcc, align = "hv",
          labels = c("a", "b"), nrow = 2)

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES,
              "supplement/masks_evapquant_ipcc.png"), width = 8, height = 8)
