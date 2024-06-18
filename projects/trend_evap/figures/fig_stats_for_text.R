# Figure 1 - prep the global overview ----

source('source/evap_trend.R')
source('source/geo_functions.R')


# Figure 1 ----
## Global quartile fold ----
evap_annual_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_1_a_global_evap_trend.rds"))

Q25_global <- evap_annual_trend[, quantile(slope, 0.25)]
Q75_global <- evap_annual_trend[, quantile(slope, 0.75)]
Q75_global/Q25_global


## Area stats ----

evap_trend_stats <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_1_b_c_grid_quartile_stats.rds"))

grid_cell_area <- unique(evap_trend_stats[, .(lon, lat)]) %>% grid_area() # m2
evap_trend_stats <- grid_cell_area[evap_trend_stats, on = .(lon, lat)]
total_area <- evap_trend_stats[, sum(area)]

### Fold summaries ----
evap_trend_stats[, sum(area)/total_area, .(fold_brk)]
evap_trend_stats[, sum(area)/total_area, .(fold_brk_detailed)]

### sign difference ----
evap_trend_stats[, sum(area)/total_area, .(sign)]


# Figure 2  ----
## Opposing trends ----
evap_index <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_fig_2_a_c_d_grid_trend_stats.rds"))

evap_index <- grid_cell_area[evap_index, on = .(lon, lat)]
p_val_opposing <- evap_index[,.(p_area = sum(area)), (p_val_opposing)]
p_val_opposing[, total_area := sum(p_area)]
p_val_opposing[, fraction := p_area/total_area]

p_val_opposing
p_val_opposing[order(p_val_opposing)]

# most trends are significant ----
sig_trends <- evap_index[,.(p_area = sum(area)), (more_sig_trends)]
sig_trends[, total_area := sum(p_area)]
sig_trends[, fraction := p_area/total_area]

sig_trends
sig_trends[order(sig_trends)]

# DCI
DCI_all <- evap_index[,.(p_area = sum(area)), (DCI_all_brk)]
DCI_all[, total_area := sum(p_area)]
DCI_all[, fraction := p_area/total_area]

DCI_all
DCI_all[order(DCI_all)]

## Figure 3 ----
evap_trend_area_dataset <- readRDS(paste0(PATH_SAVE_EVAP_TREND_TABLES, "data_SI_fig_3_area_fraction_trend_significance_by_product.rds"))
