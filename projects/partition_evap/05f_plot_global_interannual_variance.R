# Boxplot of global interannual variance between products ----
source('source/partition_evap.R')

## Data ----
evap_annual_vol <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_global_annual_mean.rds"))
evap_dataset_means <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_datasets.rds"))
dataset_types <- unique(evap_dataset_means[, .(dataset, dataset_type)])
evap_annual_vol <- evap_annual_vol[dataset_types, on = .(dataset)]
evap_annual_vol[, dataset_type := factor(dataset_type, levels =  c( "reanalysis", "remote sensing","hydrologic model","ensemble"), 
                                              labels = c( "Reanalyses", "Remote sensing","Hydrologic model","Ensemble"))]
## Figure ----
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
               "mod16a" = "green",
               "terraclimate" = "darkblue"
)

ggplot(evap_annual_vol, aes(x = 0, y = evap_mean)) +
  geom_boxplot(fill = NA, aes(x = dataset_type, col = dataset), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Evaporation [mm/year]')) +
  scale_color_manual(values = cols_data) + 
  guides(col = guide_legend(title = "Dataset"), lty = guide_legend(title = "Dataset")) +
  theme_bw() +
  theme(axis.text.x = element_blank())

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "fig1_evap_datasets_global_annual_mm.png"), 
       width = 8, height = 8)



ggplot(evap_annual_vol, aes(x = 0, y = evap_volume)) +
  geom_boxplot(fill = NA, aes(x = dataset_type, col = dataset), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Global annual volume [m3]')) +
  scale_color_manual(values = cols_data) + 
  guides(col = guide_legend(title = "Dataset"), lty = guide_legend(title = "Dataset")) +
  theme_bw() +
  theme(axis.text.x = element_blank())

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "fig1_evap_datasets_global_annual_volume.png"), 
       width = 8, height = 8)

trans_fac <- evap_annual_vol[, evap_mean/evap_volume][1]


ggplot(evap_annual_vol, aes(x = 0, y = evap_volume)) +
  geom_boxplot(fill = NA, aes(x = dataset_type, col = dataset), lwd = 0.7, position = "identity") +
  geom_violin(fill = NA, lwd = 0.7) +
  geom_boxplot(fill = NA, lwd = 0.7, col = "gray80") +
  scale_x_discrete(name = "") +
  labs(y = expression(paste('Global annual volume [m'^3,']')))+
  scale_y_continuous(sec.axis = sec_axis(~ . *trans_fac, name = "Global annual evaporation [mm]"))+
  scale_color_manual(values = cols_data) + 
  guides(col = guide_legend(title = "Dataset"), lty = guide_legend(title = "Dataset")) +
  theme_bw() +
  theme(axis.text.x = element_blank())

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "fig1_evap_datasets_global_annual_volume_mm.png"), 
       width = 8, height = 8)

