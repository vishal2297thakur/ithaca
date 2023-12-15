# Exploratory maps of mean annual evaporation per product and anomaly -----

source('source/partition_evap.R')

## Data ----
evap_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets.rds"))

evap_mean_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_datasets.rds"))
evap_volume <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_volume_grid.rds"))

evap_annual <-readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_global_annual_mean.rds"))
evap_annual <- evap_annual[!(dataset == "etmonitor" & year == "2000")]

datasets <- evap_mean_datasets[, unique(dataset)]



## Plotting evap means ----
ggplot(evap_mean_datasets[dataset %in% c(datasets[1:13])])+
  geom_tile(aes(x = lon, y = lat, col = evap_mean))+
  scale_color_binned(type = "viridis", 
                    breaks = c(-50,0, 0.5, seq(100, 700, 150), 1000, 2000, 2800), 
                    limits = c(-50, 2800),
                    direction = -1)+
  facet_wrap(~dataset, nrow = 2)+
  theme_bw()

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "evap_datasets_mean_annual_mm.png"), 
       width = 30, height = 8)

## Plotting evap sd ----

ggplot(evap_mean_datasets[dataset %in% c(datasets[1:13])])+
  geom_tile(aes(x = lon, y = lat, col = evap_sd))+
  scale_color_binned(type = "viridis", 
                     breaks = c(seq(0, 100, 20), 500), 
                     limits = c(0, 200),
                     direction = -1)+
  facet_wrap(~dataset, nrow = 2)+
  theme_bw()

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "evap_datasets_sd_mm.png"), 
       width = 30, height = 8)


## Plotting evap global means ----
cols_data <- c("bess" = "chartreuse2",
               "camele" = "red",
               "era5-land" = "gold1",
               "etmonitor" = "chartreuse4",
               "etsynthesis" = "hotpink",
               "fldas" = "darkslategray1",
               "gldas-clsm" = "deepskyblue1",
               "gldas-noah" = "deepskyblue3",
               "gldas-vic" = "deepskyblue4",
               "gleam" = "darkgreen",
               "jra55" = "orange1",
               "merra2" = "orange3",
               "terraclimate" = "darkblue"
)


ggplot(evap_annual)+
  geom_line(aes(x = year, y = evap_mean, col = dataset))+
  scale_color_manual(values = cols_data) + 
  theme_bw()

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "evap_datasets_global_mean.png"), 
       width = 8, height = 8)


ggplot(evap_annual)+
  geom_line(aes(x = year, y = evap_volume, col = dataset))+
  scale_color_manual(values = cols_data) + 
  theme_bw()

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "evap_datasets_global_mean_volume.png"), 
       width = 8, height = 8)
## Plotting evap global anomaly ----

evap_annual[, evap_anomaly := evap_mean - mean(evap_mean), .(dataset)]

ggplot(evap_annual)+
  geom_line(aes(x = year, y = evap_anomaly, col = dataset))+
  scale_color_manual(values = cols_data) + 
  theme_bw()
ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "evap_datasets_global_anomaly.png"), 
       width = 8, height = 8)
