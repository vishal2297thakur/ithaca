# Plot number of stations per grid cell

source('source/partition_prec.R')
source('source/geo_functions.R')
source('source/graphics.R')

# Data
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))
prec_grid <- read_stars(paste0(PATH_SAVE_PARTITION_PREC,
                               "prec_station_grid.nc")) %>% st_as_sf()
colnames(prec_grid)[1] <- "value"
st_crs(prec_grid) <- "+proj=longlat +datum=WGS84 +no_defs"
world_sf <- ne_countries(returnclass = "sf")
prec_mask_sf <- prec_mask[, .(lon, lat, rel_dataset_agreement)
][, value := as.numeric(rel_dataset_agreement)]
prec_mask_sf <- prec_mask_sf[, .(lon, lat, value)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

#Figure

p00 <- ggplot(prec_grid) +
  geom_sf(data = world_sf, fill = NA) +
  geom_sf(aes(color = value)) +
  scale_color_viridis_c(option = "H") +
  labs(x = "Lon", y = "Lat", color = "No.\nStations") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  #scale_y_continuous(breaks = seq(-60, 60, 30)) +
  #scale_x_continuous(breaks = seq(-150, 150, 30)) +
  theme_bw() +
  theme_opts +
  theme(panel.border = element_blank(),
        axis.ticks.length =unit(0, "cm"))

p00 <- ggplot(prec_mask_sf) +
  geom_sf(data = world_sf, fill = NA) +
  geom_sf(aes(color = factor(value), fill = factor(value))) +
  scale_fill_manual(values = colset_mid[8:4],
                    labels = levels(prec_mask$rel_dataset_agreement)) +
  scale_color_manual(values = colset_mid[8:4],
                     labels = levels(prec_mask$rel_dataset_agreement),
                     guide = "none") +
  labs(x = "Lon", y = "Lat", fill = "Dataset\nAgreement") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  theme_bw() +
  theme_opts +
  theme(panel.border = element_blank(),
        axis.ticks.length =unit(0, "cm"))
