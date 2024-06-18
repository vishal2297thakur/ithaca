# Pair-wise dataset comparison ----

source('source/partition_evap.R')

test_out <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "ks_test_gridwise.rds"))
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
evap_datasets_grid_mean <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_grid_mean.rds"))
evap_grid <- evap_datasets_grid_mean[, .(lat, lon, area)]
evap_grid <- unique(evap_grid)
test_out_area <- merge(test_out, evap_grid, by = c("lat", "lon"), all.x = T)


test_out_area[KS_test_p >= 0.05, match := 1]
test_out_area[KS_test_p < 0.05, match := 0]

saveRDS(test_out_area, paste0(PATH_SAVE_PARTITION_EVAP, "ks_test_gridwise_area.rds"))

## global ----
product <- test_out_area[, .(area_match = sum(match*area), area_total = sum(area)), .(dataset.x, dataset.y)]
product[, fraction := area_match/area_total]

saveRDS(product, paste0(PATH_SAVE_PARTITION_EVAP, "area_fraction_matching_products.rds"))

## landcover ----

land_cover <- merge(test_out_area, evap_mask[,.(lon, lat, land_cover_short_class)], by = c("lon", "lat"), all.x = T)
land_cover_area <- land_cover[, .(area_match = sum(match*area), area_total = sum(area)), .(dataset.x, dataset.y, land_cover_short_class)]
land_cover_area[, fraction := area_match/area_total]
saveRDS(land_cover_area, paste0(PATH_SAVE_PARTITION_EVAP, "area_fraction_matching_products_land_cover.rds"))

## biomes ----
biome <- merge(test_out_area, evap_mask[,.(lon, lat, biome_short_class)], by = c("lon", "lat"), all.x = T)
biome_area <- biome[, .(area_match = sum(match*area), area_total = sum(area)), .(dataset.x, dataset.y, biome_short_class)]
biome_area[, fraction := area_match/area_total]
saveRDS(biome_area, paste0(PATH_SAVE_PARTITION_EVAP, "area_fraction_matching_products_biome.rds"))

## ipccs ----
ipcc <- merge(test_out_area, evap_mask[,.(lon, lat, IPCC_ref_region)], by = c("lon", "lat"), all.x = T)
ipcc_area <- ipcc[, .(area_match = sum(match*area), area_total = sum(area)), .(dataset.x, dataset.y, IPCC_ref_region)]
ipcc_area[, fraction := area_match/area_total]
saveRDS(ipcc_area, paste0(PATH_SAVE_PARTITION_EVAP, "area_fraction_matching_products_ipcc.rds"))

## elevation ----
elev <- merge(test_out_area, evap_mask[,.(lon, lat, elev_class)], by = c("lon", "lat"), all.x = T)
elev_area <- elev[, .(area_match = sum(match*area), area_total = sum(area)), .(dataset.x, dataset.y, elev_class)]
elev_area[, fraction := area_match/area_total]
saveRDS(elev_area, paste0(PATH_SAVE_PARTITION_EVAP, "area_fraction_matching_products_elev.rds"))

## evap quantiles ----
evap <- merge(test_out_area, evap_mask[,.(lon, lat, evap_quant)], by = c("lon", "lat"), all.x = T)
evap_area <- evap[, .(area_match = sum(match*area), area_total = sum(area)), .(dataset.x, dataset.y, evap_quant)]
evap_area[, fraction := area_match/area_total]
saveRDS(evap_area, paste0(PATH_SAVE_PARTITION_EVAP, "area_fraction_matching_products_evap.rds"))

##