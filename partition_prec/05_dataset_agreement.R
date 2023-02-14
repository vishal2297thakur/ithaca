# Plot global map of dataset agreement classses 
library(rnaturalearth)

source('source/partition_prec.R')
source('source/geo_functions.R')
source('source/graphics.R')

# Data
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))
levels(prec_mask$rel_dataset_agreement) <- c("High", "Above average", "Average", "Below average", "Low")

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

# Figures
ggplot(prec_grid) +
  geom_sf(data = world_sf, fill = NA) +
  geom_sf(color = "dark red") +
  scale_color_viridis_c(option = "H") +
  labs(x = "", y = "", color = "No.\nStations") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.ticks.length =unit(0, "cm"))+
  theme(panel.grid.major = element_line(colour="dark grey"))

ggplot(prec_mask_sf) +
  geom_sf(data = world_sf, fill = NA) +
  geom_sf(aes(color = factor(value), fill = factor(value))) +
  scale_fill_manual(values = colset_RdBu_5,
                    labels = levels(prec_mask$rel_dataset_agreement)) +
  scale_color_manual(values = colset_RdBu_5,
                     labels = levels(prec_mask$rel_dataset_agreement),
                     guide = "none") +
  labs(fill = "Dataset\nAgreement") +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  theme_bw() +
  theme_opts +
  theme(panel.border = element_blank(),
        axis.ticks.length =unit(0, "cm"))

ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "03_partition_volume_climate.png"), width = 12, height = 15)