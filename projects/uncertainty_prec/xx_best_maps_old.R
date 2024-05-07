# Maps
source("source/uncertainty_prec.R")

install.packages(setdiff("rnaturalearth", rownames(installed.packages())))
library(rnaturalearth)
library(sf)
library(stars)

## Data
prec_masks <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC_SPATIAL,
                             "pRecipe_masks.rds"))

### Borders and labels
earth_box <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC_SPATIAL,
                            "earth_box.rds")) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84 +no_defs")

world_sf <- ne_countries(returnclass = "sf")
borders_sf <- read_sf("~/shared/data/geodata/world_borders/border_shapefile.shp")
ipcc_sf <- read_sf("~/shared/data/geodata/ipcc_v4/IPCC-WGI-reference-regions-v4.shp")
basin_sf <- read_sf("~/shared/data/geodata/major_basins/Major_Basins_of_the_World.shp")
kg_sf <- read_sf("~/shared/data/geodata/kg_classes/shapefile_kg.shp")
kg_sf <- kg_sf[kg_sf$layer != "Ocean",]

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

### Countries
prec_data <- fread(paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,
                          "country_ranking.csv"))

DATASETS <- c(unique(prec_data$dataset), NA) %>% as.factor()
levels(DATASETS) <- c(levels(DATASETS), "Others")
DATASETS[is.na(DATASETS)] <- "Others"

TOP_DATASETS <- c("gpm-imerg", "mswep", "merra2", "gsmap", "em-earth", "gpcp",
                  "era5-land", "precl", "persiann", "terraclimate", "fldas", "cru-ts") %>%
  factor(levels(DATASETS))

prec_data <- prec_data[, .SD[which.max(prec_t)], by = .(country)
                       ][country != ""]
prec_data <- merge(prec_masks[, .(lon, lat, country)], prec_data,
                   by = "country")
prec_data <- prec_data[!(dataset %in% TOP_DATASETS), dataset := "Others"
                       ][, .(lon, lat, dataset = factor(dataset,
                                 levels(DATASETS)))]

to_plot_country <- prec_data[, .(lon, lat, name = as.numeric(dataset))] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_country$name <- levels(DATASETS)[to_plot_country$name] %>%
  factor(levels(DATASETS))

p01 <- ggplot(to_plot_country) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = name, fill = name)) +
  geom_sf(data = world_sf, fill = NA, color = "gray23") +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_fill_manual(values = c("gpm-imerg" = "#1f78b4", "mswep" = "#cab2d6",
                               "merra2" = "#b2df8a", "gsmap" = "#a6cee3",
                               "em-earth" = "#ff7f00", "gpcp" = "#fdbf6f",
                               "era5-land" = "#33a02c", "precl" = "#fb9a99",
                               "persiann" = "#e31a1c", "Others" = "gray23",
                               "terraclimate" = "#6a3d9a", "fldas" = "#ffff99",
                               "cru-ts" = "#b15928"), drop = FALSE,
                    labels = c("gpm-imerg" = "GPM-IMERG v7", "precl"= "PREC/L", "cru-ts" = "CRU TS v4.06",
                               "mswep" = "MSWEP v2.8", "merra2" = "MERRA-2",
                               "gsmap" = "GSMaP v8", "em-earth" = "EM-Earth",
                               "gpcp" = "GPCP v3.2", "era5-land" = "ERA5-Land",
                               "persiann" = "PERSIANN-CDR", "fldas" = "FLDAS",
                               "terraclimate" = "TerraClimate")) +
  scale_color_manual(values = c("gpm-imerg" = "#1f78b4", "mswep" = "#cab2d6",
                                "merra2" = "#b2df8a", "gsmap" = "#a6cee3",
                                "em-earth" = "#ff7f00", "gpcp" = "#fdbf6f",
                                "era5-land" = "#33a02c", "precl" = "#fb9a99",
                                "persiann" = "#e31a1c", "Others" = "gray23",
                                "terraclimate" = "#6a3d9a", "fldas" = "#ffff99", "cru-ts" = "#b15928"),
                     guide = "none") +
  geom_sf(data = world_sf, fill = NA, color = "gray23") +
  labs(x = NULL, y = NULL, fill = "Dataset") +
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
prec_data <- fread(paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,
                          "ipcc_ranking.csv"))

prec_data <- prec_data[, .SD[which.max(prec_t)], by = .(ipcc_region)]

prec_data <- merge(prec_masks[ipcc_short_region != "NAO" &
                                ipcc_short_region != "EAO" &
                                ipcc_short_region != "SOO" &
                                ipcc_short_region != "SIO" &
                                ipcc_short_region != "EIO" &
                                ipcc_short_region != "BOB" &
                                ipcc_short_region != "ARS" &
                                ipcc_short_region != "NPO" &
                                ipcc_short_region != "EPO" &
                                ipcc_short_region != "SPO",
                              .(lon, lat, ipcc_region)], prec_data,
                   by = "ipcc_region")

prec_data <- prec_data[!(dataset %in% TOP_DATASETS), dataset := "Others"
][, .(lon, lat, dataset = factor(dataset,
                                 levels(DATASETS)))]

to_plot_ipcc <- prec_data[, .(lon, lat, name = as.numeric(dataset))] %>%
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

to_plot_ipcc$name <- levels(DATASETS)[to_plot_ipcc$name] %>%
  factor(levels(DATASETS))

p02 <- ggplot(to_plot_ipcc) +
  geom_sf(data = world_sf, fill = "gray69", color = "gray69") +
  geom_sf(aes(color = name, fill = name)) +
  geom_sf(data = ipcc_sf, fill = NA, color = "gray23") +
  geom_sf(data = borders_sf, fill = NA, color = "gray23") +
  geom_sf_text(data = ipcc_sf, aes(label = Acronym), size = 2,
               fontface = "bold") +
  geom_sf(data = earth_box, fill = NA, color = "gray23", lwd = 2) +
  scale_fill_manual(values = c("gpm-imerg" = "#1f78b4", "mswep" = "#cab2d6",
                               "merra2" = "#b2df8a", "gsmap" = "#a6cee3",
                               "em-earth" = "#ff7f00", "gpcp" = "#fdbf6f",
                               "era5-land" = "#33a02c", "precl" = "#fb9a99",
                               "persiann" = "#e31a1c", "Others" = "gray23",
                               "terraclimate" = "#6a3d9a", "fldas" = "#ffff99", "cru-ts" = "#b15928"),
                    labels = c("gpm-imerg" = "GPM-IMERG v7", "precl"= "PREC/L", "cru-ts" = "CRU TS v4.06",
                               "mswep" = "MSWEP v2.8", "merra2" = "MERRA-2",
                               "gsmap" = "GSMaP v8", "em-earth" = "EM-Earth",
                               "gpcp" = "GPCP v3.2", "era5-land" = "ERA5-Land",
                               "persiann" = "PERSIANN-CDR", "fldas" = "FLDAS",
                               "terraclimate" = "TerraClimate")) +
  scale_color_manual(values = c("gpm-imerg" = "#1f78b4", "mswep" = "#cab2d6",
                                "merra2" = "#b2df8a", "gsmap" = "#a6cee3",
                                "em-earth" = "#ff7f00", "gpcp" = "#fdbf6f",
                                "era5-land" = "#33a02c", "precl" = "#fb9a99",
                                "persiann" = "#e31a1c", "Others" = "gray23",
                                "terraclimate" = "#6a3d9a", "fldas" = "#ffff99", "cru-ts" = "#b15928"),
                     guide = "none") +
  labs(x = NULL, y = NULL, fill = "Dataset") +
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
p00 <- ggarrange(p01, p02, p03, p04, ncol = 2, nrow = 2, common.legend = TRUE,
                 legend = "right", labels = c("(a)", "(b)", "(c)", "(d)"))

ggsave(plot = p00,
       paste0(PATH_SAVE_UNCERTAINTY_PREC_FIGURES,
              "maps_first_rank.png"), width = 4.5*GOLDEN_RATIO*2,
       height = 4.5*2)
