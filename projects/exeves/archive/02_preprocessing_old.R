source('source/exeves.R')
require(quantreg)

region <- 'czechia'

# Evaporation
evap <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_evap.rds'))
evap <- evap[order(lon, lat)]
evap[, `:=`(grid_id, .GRP), by = list(lat, lon)]

evap_grid <- unique(evap[, .(lon, lat, grid_id)])
evap <- evap[, .(grid_id, date, value)]

## Pentads
pentads <- copy(evap)
pentads[, pentad := ceiling((yday(date) - leap_year(year(date)) * (yday(date) > 59)) / 5 )]
pentads[, `:=`(pentad_time, .GRP), by = list(pentad, year(date))]
pentads[, pentad_mean := mean(value), .(grid_id, pentad)]
pentads[, pentad_value := mean(value), .(grid_id, pentad_time)]
pentads[, pentad_q90 := quantile(value, EXTREMES_THRES), .(grid_id, pentad)]

pentads[, pentad_median_qr := rq(value ~ date,  #quantile regression: non-stationarity
                                 tau = 0.5)$fitted, by = .(grid_id, pentad)]
pentads[, pentad_q90_qr := rq(value ~ date, 
                                 tau = EXTREMES_THRES)$fitted, by = .(grid_id, pentad)]

pentads[, value := NULL]
pentads_qr <- unique(pentads[, .(grid_id, pentad, pentad_time, value)])
pentads_qr[, pentad_median_qr := rq(pentad_value ~ pentad_time,  #quantile regression: non-stationarity
                                      tau = 0.5)$fitted, by = .(grid_id, pentad)]
pentads_qr[, pentad_q90_qr := rq(pentad_value ~ pentad_time, 
                                   tau = EXTREMES_THRES)$fitted, by = .(grid_id, pentad)]
pentads <- merge(pentads, pentads_qr, all = TRUE, by = c("grid_id", "pentad_time",  "pentad_value"))
rm(pentads_qr); gc()

## Events
### Stationarity assumption
exeves <- merge(evap, pentads[, .(grid_id, date, pentad_mean, pentad_q90)], all.x = TRUE, by = c("grid_id", "date"))
exeves[, evap_event := FALSE]
exeves[, value_above_mean := FALSE]
exeves[, extreme := FALSE]
exeves[value > pentad_mean, value_above_mean := TRUE]
exeves[value > pentad_q90, extreme := TRUE]
exeves[, above_mean_id := rleid(value_above_mean)]
exeves[, extreme_id := rleid(extreme), .(grid_id)]

exeves[extreme == TRUE, evap_event := TRUE, .(grid_id, above_mean_id)]
above_mean_ids_with_extreme <- exeves[extreme == TRUE, above_mean_id]
exeves[above_mean_id %in% above_mean_ids_with_extreme, evap_event := TRUE]
exeves[, event_id := rleid(evap_event), .(grid_id)]
exeves[evap_event != TRUE, event_id := NA]
exeves[extreme != TRUE, extreme_id := NA]

### Non-stationarity assumption
exeves_qr <- merge(evap, pentads[, .(grid_id, date, pentad_median_qr, pentad_q90_qr)], all.x = TRUE, by = c("grid_id", "date"))
exeves_qr[, evap_event := FALSE]
exeves_qr[, value_above_mean := FALSE]
exeves_qr[, extreme := FALSE]
exeves_qr[value > pentad_median_qr, value_above_mean := TRUE]
exeves_qr[value > pentad_q90_qr, extreme := TRUE]
exeves_qr[, above_mean_id := rleid(value_above_mean)]
exeves_qr[, extreme_qr_id := rleid(extreme), .(grid_id)]

exeves_qr[extreme == TRUE, evap_event := TRUE, .(grid_id, above_mean_id)]
above_mean_ids_with_extreme <- exeves_qr[extreme == TRUE, above_mean_id]
exeves_qr[above_mean_id %in% above_mean_ids_with_extreme, evap_event := TRUE]
exeves_qr[, event_qr_id := rleid(evap_event), .(grid_id)]
exeves_qr[evap_event != TRUE, event_qr_id := NA]
exeves_qr[extreme != TRUE, extreme_qr_id := NA]

exeves <- merge(exeves[, .(grid_id, date, value, event_id, extreme_id)], 
      exeves_qr[, .(grid_id, date, value, event_qr_id, extreme_qr_id)], by = c('grid_id', 'date', 'value'))
      
saveRDS(evap_grid, paste0(PATH_OUTPUT_DATA, "grid_", region, ".rds"))
saveRDS(evap, paste0(PATH_OUTPUT_DATA, region, '_evap_grid.rds'))
saveRDS(pentads, paste0(PATH_OUTPUT_DATA, 'pentads_', region, '.rds'))
saveRDS(exeves, paste0(PATH_OUTPUT_DATA, 'exeves_', region, '.rds'))

rm(evap); rm(pentads); rm(exeves); gc()

# Precipitation
prec <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_prec.rds'))

prec <- evap_grid[prec, on = .(lon, lat)][, lon := NULL][, lat := NULL]
prec[, q90 := quantile(value, 0.9), grid_id]
prec[value > q90, extreme_prec := TRUE][, q90 := NULL]

saveRDS(prec, paste0(PATH_OUTPUT_DATA, region, '_prec_grid.rds'))

#rad
lwrad <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_lwrad.rds'))
swrad <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_swrad.rds'))

lwrad <- evap_grid[lwrad, on = .(lon, lat)][, lon := NULL][, lat := NULL]
swrad <- evap_grid[swrad, on = .(lon, lat)][, lon := NULL][, lat := NULL]

saveRDS(lwrad, paste0(PATH_OUTPUT_DATA, region, '_lwrad_grid.rds'))
saveRDS(swrad, paste0(PATH_OUTPUT_DATA, region, '_swrad_grid.rds'))


