# Rank datasets according to signal ----
source('source/evap_trend.R')
source('source/geo_functions.R')

## Data ----
### Input Data generated in projects/partition_evap/04
PATH_SAVE_PARTITION_EVAP <- paste0(PATH_SAVE, "partition_evap/")
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))

### Input data generated in trend_evap/bootstrap/01_c 
evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_trends_summary_p_val_lon_lat_dataproducts.rds"))


### land use
land_use <- merge(evap_mask[, .(lat, lon, land_cover_short_class)], 
                    evap_trend[, .(lon, lat, year, area, 
                                   N_pos_0_01, N_pos_0_05, N_pos_0_1, N_pos_0_2, N_pos_all, 
                                   N_neg_0_01, N_neg_0_05, N_neg_0_1, N_neg_0_2, N_neg_all, 
                                   N_none_0_01, N_none_0_05, N_none_0_1, N_none_0_2, dataset)], 
                          by = c("lon", "lat"))


land_use_sums_area <- land_use[, .(sum_N_pos_0_01 = sum(N_pos_0_01*area),
                                         sum_N_pos_0_05 = sum(N_pos_0_05*area),
                                         sum_N_pos_0_1 = sum(N_pos_0_1*area),
                                         sum_N_pos_0_2 = sum(N_pos_0_2*area),
                                         sum_N_pos_all = sum(N_pos_all*area),
                                         sum_N_neg_0_01 = sum(N_neg_0_01*area),
                                         sum_N_neg_0_05 = sum(N_neg_0_05*area),
                                         sum_N_neg_0_1 = sum(N_neg_0_1*area),
                                         sum_N_neg_0_2 = sum(N_neg_0_2*area),
                                         sum_N_neg_all = sum(N_neg_all*area),
                                         sum_N_none_0_01 = sum(N_none_0_01*area),
                                         sum_N_none_0_05 = sum(N_none_0_05*area),
                                         sum_N_none_0_1 = sum(N_none_0_1*area),
                                         sum_N_none_0_2 = sum(N_none_0_2*area)
), .(dataset, land_cover_short_class)]


evap_sums_melt <- melt(land_use_sums_area,
                       measure.vars = c("sum_N_pos_0_01",
                                        "sum_N_pos_0_05",
                                        "sum_N_pos_0_1",
                                        "sum_N_pos_0_2",
                                        "sum_N_pos_all",
                                        "sum_N_neg_0_01",
                                        "sum_N_neg_0_05",
                                        "sum_N_neg_0_1",
                                        "sum_N_neg_0_2",
                                        "sum_N_neg_all",
                                        "sum_N_none_0_01",
                                        "sum_N_none_0_05",
                                        "sum_N_none_0_1",
                                        "sum_N_none_0_2"
                       ))


evap_sums_melt[, rank_datasets := rank(-value, ties = "first"), .(variable, land_cover_short_class)]

saveRDS(evap_sums_melt, paste0(PATH_SAVE_EVAP_TREND, "land_use_ranked_datasets_signal_booster_p_thresholds_bootstrap.rds"))
