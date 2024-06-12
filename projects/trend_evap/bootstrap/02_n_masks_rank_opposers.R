# Rank datasets according to signal ----
source('source/evap_trend.R')
source('source/geo_functions.R')


## Data ----
## Created in trend_evap/01_g
evap_trend_g <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_DCI_trend_groups_p_thresholds_bootstrap.rds"))

## Created in trend_evap/01_h
evap_trend_h <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_DCI_trend_groups_p_thresholds_bootstrap_dataset_leftout.rds"))

### Input Data generated in projects/partition_evap/04
PATH_SAVE_PARTITION_EVAP <- paste0(PATH_SAVE, "partition_evap/")
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))

## Estimate opposing fraction of all data ----
## add area estimate 
grid_cell_area <- unique(evap_trend_g[, .(lon, lat)]) %>% grid_area() # m2

evap_trend_g  <- grid_cell_area[evap_trend_g, on = .(lon, lat)]


### land use
land_use <- merge(evap_mask[, .(lat, lon, land_cover_short_class)], 
                  evap_trend_g[, .(lon, lat, trend_0_01, trend_0_05, trend_0_1, trend_0_2, trend_all, area)], 
                  by = c("lon", "lat"))

setnames(land_use, old = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all"), 
         new = c("p < 0.01", " p < 0.05", "p < 0.1", "p < 0.2", "all"))

land_use_melt <- melt(land_use, measure.vars = c("p < 0.01", " p < 0.05", "p < 0.1", "p < 0.2", "all"))

total_area_land <- land_use_melt[, .(total_area = sum(area)), .(land_cover_short_class, variable)]
total_area_land <- total_area_land[, .(total_area = unique(total_area)), .(land_cover_short_class)]

evap_sum_opposing <- land_use_melt[value == "opposing", .(sum_var = sum(area)), .(variable, land_cover_short_class)]
evap_sum_opposing <- merge(evap_sum_opposing, total_area_land, by = "land_cover_short_class")
evap_sum_opposing[, fraction := sum_var/total_area]

## Estimate opposing fraction dataset leftout
land_use_leftout <- merge(evap_mask[, .(lat, lon, land_cover_short_class)], 
                  evap_trend_h[, .(lon, lat, trend_0_01, trend_0_05, trend_0_1, trend_0_2, trend_all, dataset_leftout)], 
                  by = c("lon", "lat"))

land_use_leftout  <- grid_cell_area[land_use_leftout, on = .(lon, lat)]
setnames(land_use_leftout, old = c("trend_0_01","trend_0_05", "trend_0_1","trend_0_2","trend_all"), 
         new = c("p < 0.01", " p < 0.05", "p < 0.1", "p < 0.2", "all"))

land_use_leftout_melt <- melt(land_use_leftout, measure.vars = c("p < 0.01", " p < 0.05", "p < 0.1", "p < 0.2", "all"))

land_use_leftout_sum_opposing <- land_use_leftout_melt[value == "opposing", .(sum_leftout = sum(area)), .(variable, dataset_leftout, land_cover_short_class)]
land_use_leftout_sum_opposing <- merge(land_use_leftout_sum_opposing, total_area_land, by = "land_cover_short_class")
land_use_leftout_sum_opposing[, fraction_leftout := sum_leftout/total_area]

land_opposing <- merge(land_use_leftout_sum_opposing, evap_sum_opposing, by = c("variable", "land_cover_short_class", "total_area"))

land_opposing[, fraction_diff := fraction- fraction_leftout]

land_opposing[, rank_opp := rank(-fraction_diff, ties = "first"), .(variable, land_cover_short_class)]

## Save data ----
saveRDS(land_opposing, paste0(PATH_SAVE_EVAP_TREND, "land_use_datasets_opposing_p_thresholds_bootstrap.rds"))
