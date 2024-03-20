# Trend magnitude per class ----
source('source/evap_trend.R')
source('source/graphics.R')

## Data ----
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_evap_area.rds"))  

evap_trend[dataset %in% EVAP_DATASETS_REANAL, dataset_type := 'Reanalysis'
][dataset %in% EVAP_DATASETS_REMOTE, dataset_type := 'Remote sensing'
][dataset %in% EVAP_DATASETS_HYDROL, dataset_type := 'Hydrologic model'
][dataset %in% EVAP_DATASETS_ENSEMB, dataset_type := 'Ensemble']

evap_dataset_means <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_datasets.rds"))

evap_trend <- merge(evap_trend, evap_dataset_means[,.(lon, lat, dataset, evap_mean)], by = c("lon", "lat", "dataset"), all.x = T)

## Analysis ----

evap_trend[, dataset := factor(dataset, 
                                         levels = c("bess", "etmonitor", "gleam",
                                                    "camele", "etsynthesis", 
                                                    "fldas", "gldas-clsm", "gldas-noah", "gldas-vic", "terraclimate",
                                                    "era5-land", "jra55","merra2"),
                                         labels = c("bess", "etmonitor", "gleam",
                                                    "camele", "etsynthesis", 
                                                    "fldas", "gldas-clsm", "gldas-noah", "gldas-vic", "terraclimate",
                                                    "era5-land", "jra55","merra2")
                                         , ordered = TRUE)]
evap_trend[, evap_trend_volume := area * M2_TO_KM2 * theil_sen_slope * MM_TO_KM]
evap_trend[, evap_volume := area * M2_TO_KM2 * evap_mean * MM_TO_KM]


land_cover_class_global <- evap_trend[, .(evap_trend_volume = sum(evap_trend_volume), area = sum(area), 
                                          evap_volume = sum(evap_volume, na.rm = T)), 
                                            .(dataset, dataset_type, land_cover_short_class)]

land_cover_class_global[, evap_trend_mean := ((evap_trend_volume / M2_TO_KM2) / area) / MM_TO_KM]
land_cover_class_global[, evap_mean := ((evap_volume / M2_TO_KM2) / area) / MM_TO_KM]

biome_class_global <- evap_trend[, .(evap_trend_volume = sum(evap_trend_volume), area = sum(area)), 
                                      .(dataset, dataset_type, biome_class)]

biome_class_global[, evap_trend_mean := ((evap_trend_volume / M2_TO_KM2) / area) / MM_TO_KM]

### Biome types
biome_class_global[grepl("Tundra", biome_class) == TRUE, biome_short_class := "Tundra"]
biome_class_global[grepl("Boreal Forests", biome_class) == TRUE, biome_short_class := "B. Forests"]
biome_class_global[grepl("Dry Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Dry BL Forests"]
biome_class_global[grepl("Moist Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Moist BL Forests"]
biome_class_global[grepl("Subtropical Coniferous Forests", biome_class) == TRUE, biome_short_class := "T/S Coni. Forests"]
biome_class_global[grepl("Temperate Conifer Forests", biome_class) == TRUE, biome_short_class := "T. Coni. Forests"]
biome_class_global[grepl("Temperate Broadleaf & Mixed Forests", biome_class) == TRUE, biome_short_class := "T. BL Forests"]
biome_class_global[grepl("Temperate Grasslands", biome_class) == TRUE, biome_short_class := "T. Grasslands"]
biome_class_global[grepl("Subtropical Grasslands", biome_class) == TRUE, biome_short_class := "T/S Grasslands"]
biome_class_global[grepl("Montane Grasslands", biome_class) == TRUE, biome_short_class := "M. Grasslands"]
biome_class_global[grepl("Flooded", biome_class) == TRUE, biome_short_class := "Flooded"]
biome_class_global[grepl("Mangroves", biome_class) == TRUE, biome_short_class := "Mangroves"]
biome_class_global[grepl("Deserts", biome_class) == TRUE, biome_short_class := "Deserts"]
biome_class_global[grepl("Mediterranean", biome_class) == TRUE, biome_short_class := "Mediterranean"]
biome_class_global[grepl("N/A", biome_class) == TRUE, biome_short_class := NA]
biome_class_global[, biome_short_class := factor(biome_short_class)]

elev_class_global <- evap_trend[, .(evap_trend_volume = sum(evap_trend_volume), area = sum(area)), 
                                 .(dataset, dataset_type, elev_class)]

elev_class_global[, evap_trend_mean := ((evap_trend_volume / M2_TO_KM2) / area) / MM_TO_KM]

ipcc_class_global <- evap_trend[, .(evap_trend_volume = sum(evap_trend_volume), area = sum(area)), 
                                .(dataset, dataset_type, IPCC_ref_region)]

ipcc_class_global[, evap_trend_mean := ((evap_trend_volume / M2_TO_KM2) / area) / MM_TO_KM]

ipcc_class_global[grepl("O", as.character(IPCC_ref_region)) == TRUE, ocean := "yes"]
ipcc_class_global[IPCC_ref_region %in% c("RAR", "BOB", "ARS"), ocean := "yes"]


KG_class_3_global <- evap_trend[, .(evap_trend_volume = sum(evap_trend_volume), area = sum(area)), 
                                .(dataset, dataset_type, KG_class_3)]

KG_class_3_global[, evap_trend_mean := ((evap_trend_volume / M2_TO_KM2) / area) / MM_TO_KM]

## plots ----
ggplot(land_cover_class_global, aes(x = land_cover_short_class, y = evap_trend_mean)) +
  geom_boxplot(width = 0.8, alpha = .7, show.legend = FALSE, col = "gray", fill = "gray90") +
  geom_point(fill = NA, aes(col = dataset, shape = dataset_type), position = "identity", size = 3) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Mean ET trend [mm/year/year]')) +
  scale_color_manual(values = cols_data) + 
  facet_wrap(~land_cover_short_class, scales = 'free', nrow = 1) +
  guides(col = guide_legend(title = "Dataset"), lty = guide_legend(title = "Dataset")) +
  theme_bw() +
  geom_abline(intercept = 0, slope = 0, "black")+
  theme(axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "evap_datasets_land_cover_trend_mm_year_year.png"), 
       width = 12, height = 8)

ggplot(land_cover_class_global, aes(x = land_cover_short_class, y = evap_trend_mean/evap_mean*100)) +
  geom_boxplot(width = 0.8, alpha = .7, show.legend = FALSE, col = "gray", fill = "gray90") +
  geom_point(fill = NA, aes(col = dataset, shape = dataset_type), position = "identity", size = 3) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Mean ET trend [%/year/year]')) +
  scale_color_manual(values = cols_data) + 
  facet_wrap(~land_cover_short_class, scales = 'free', nrow = 1) +
  guides(col = guide_legend(title = "Dataset"), lty = guide_legend(title = "Dataset")) +
  theme_bw() +
  geom_abline(intercept = 0, slope = 0, "black")+
  theme(axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "evap_datasets_land_cover_trend_mm_year_year.png"), 
       width = 12, height = 8)

ggplot(biome_class_global, aes(x = biome_short_class, y = evap_trend_mean)) +
  geom_boxplot(width = 0.8, alpha = .7, show.legend = FALSE, col = "gray", fill = "gray90") +
  geom_point(fill = NA, aes(col = dataset, shape = dataset_type), position = "identity", size = 3) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Mean ET trend [mm/year/year]')) +
  scale_color_manual(values = cols_data) + 
  facet_wrap(~biome_short_class, scales = 'free', nrow = 2) +
  guides(col = guide_legend(title = "Dataset"), lty = guide_legend(title = "Dataset")) +
  theme_bw() +
  geom_abline(intercept = 0, slope = 0, "black")+
  theme(axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "evap_datasets_biome_trend_mm_year_year.png"), 
       width = 12, height = 8)


ggplot(elev_class_global, aes(x = elev_class, y = evap_trend_mean)) +
  geom_boxplot(width = 0.8, alpha = .7, show.legend = FALSE, col = "gray", fill = "gray90") +
  geom_point(fill = NA, aes(col = dataset, shape = dataset_type), position = "identity", size = 3) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Mean ET trend [mm/year/year]')) +
  scale_color_manual(values = cols_data) + 
  facet_wrap(~elev_class, scales = 'free', nrow = 1) +
  guides(col = guide_legend(title = "Dataset"), lty = guide_legend(title = "Dataset")) +
  theme_bw() +
  geom_abline(intercept = 0, slope = 0, "black")+
  theme(axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "evap_datasets_elevation_trend_mm_year_year.png"), 
       width = 8, height = 8)


ggplot(ipcc_class_global, aes(x = IPCC_ref_region, y = evap_trend_mean)) +
  geom_boxplot(width = 0.8, alpha = .7, show.legend = FALSE, col = "gray", fill = "gray90") +
  geom_point(fill = NA, aes(col = dataset, shape = dataset_type), position = "identity", size = 2) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Mean ET trend [mm/year/year]')) +
  scale_color_manual(values = cols_data) + 
  facet_wrap(~IPCC_ref_region, scales = 'free', nrow = 5) +
  guides(col = guide_legend(title = "Dataset"), lty = guide_legend(title = "Dataset")) +
  theme_bw() +
  geom_abline(intercept = 0, slope = 0, "black")+
  theme(axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "evap_datasets_ipcc_trend_mm_year_year.png"), 
       width = 12, height = 12)



ggplot(ipcc_class_global[is.na(ocean),], aes(x = IPCC_ref_region, y = evap_trend_mean)) +
  geom_boxplot(width = 0.8, alpha = .7, show.legend = FALSE, col = "gray", fill = "gray90") +
  geom_point(fill = NA, aes(col = dataset, shape = dataset_type), position = "identity", size = 2) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Mean ET trend [mm/year/year]')) +
  scale_color_manual(values = cols_data) + 
  facet_wrap(~IPCC_ref_region, scales = 'free', nrow = 5) +
  guides(col = guide_legend(title = "Dataset"), lty = guide_legend(title = "Dataset")) +
  theme_bw() +
  geom_abline(intercept = 0, slope = 0, "black")+
  theme(axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "evap_datasets_ipcc_trend_mm_year_year_land.png"), 
       width = 12, height = 12)


ggplot(KG_class_3_global, aes(x = KG_class_3, y = evap_trend_mean)) +
  geom_boxplot(width = 0.8, alpha = .7, show.legend = FALSE, col = "gray", fill = "gray90") +
  geom_point(fill = NA, aes(col = dataset, shape = dataset_type), position = "identity", size = 2) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = bquote('Mean ET trend [mm/year/year]')) +
  scale_color_manual(values = cols_data) + 
  facet_wrap(~KG_class_3, scales = 'free', nrow = 5) +
  guides(col = guide_legend(title = "Dataset"), lty = guide_legend(title = "Dataset")) +
  theme_bw() +
  geom_abline(intercept = 0, slope = 0, "black")+
  theme(axis.text.x = element_blank())
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "evap_datasets_KG_class_3_trend_mm_year_year.png"), 
       width = 12, height = 12)
