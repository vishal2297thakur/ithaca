source('source/exeves.R')
require(quantreg)

# Evaporation
region <- 'random_locations'
evap <- readRDS(paste0(PATH_OUTPUT_DATA, 'evap_', region, '_gleam.rds'))

## Pentads
pentads <- copy(evap)
pentads[, pentad := ceiling((yday(date) - leap_year(year(date)) * (yday(date) > 59)) / 5 )]
pentads[, std_value := (value - mean(value)) / sd(value), by = .(pentad, KG_class_2)]
pentads[, pentad_std_q90 := quantile(std_value, EXTREMES_THRES), by = KG_class_2]
pentads[, pentad_std_q80 := quantile(std_value, 0.8), by = KG_class_2]
pentads[, pentad_std_q75 := quantile(std_value, LOW_THRES), by = KG_class_2]
pentads[, pentad_median_qr := rq(std_value ~ date,  #quantile regression: non-stationarity
                                 tau = 0.5)$fitted, by = .(pentad, KG_class_2)]
pentads[, pentad_std_q90_qr := rq(std_value ~ date, 
                                 tau = EXTREMES_THRES)$fitted, by = .(pentad, KG_class_2)]
pentads[, value := NULL]

## Events
### Mean - Quantile 0.90 definition
exeves <- merge(evap, pentads[, .(KG_class_2, date, std_value, pentad_median_qr, 
                                  pentad_std_q75, pentad_std_q80, pentad_std_q90, pentad_std_q90_qr)], 
                all.x = TRUE, by = c("KG_class_2", "date"))

exeves[, evap_event := FALSE]
exeves[, value_above_low_thres := FALSE]
exeves[, extreme := FALSE]
exeves[std_value > 0, value_above_low_thres := TRUE]
exeves[std_value > pentad_std_q90, extreme := TRUE]
exeves[, above_low_thres_id := rleid(value_above_low_thres)]
exeves[, extreme_id := rleid(extreme), .(KG_class_2)]

exeves[extreme == TRUE, evap_event := TRUE, .(KG_class_2, above_low_thres_id)] 
above_low_thres_ids_with_extreme <- exeves[extreme == TRUE, above_low_thres_id]
exeves[above_low_thres_id %in% above_low_thres_ids_with_extreme, evap_event := TRUE]
exeves[, event_id := rleid(evap_event), .(KG_class_2)]
exeves[evap_event != TRUE, event_id := NA]
exeves[extreme != TRUE, extreme_id := NA]

exeves[, period := ordered('up_to_2001')]
exeves[date > END_PERIOD_1, period := ordered('after_2001')]

exeves[month(date) < 4, season := ordered("JFM")]
exeves[month(date) >= 4 & month(date) < 7, season := ordered("AMJ")]
exeves[month(date) >= 7 & month(date) < 10, season := ordered("JAS")]
exeves[month(date) >= 10, season := ordered("OND")]

### Median - Quantile regression 0.90 definition
exeves_qr <- merge(evap, pentads[, .(KG_class_2, date, std_value, pentad_median_qr, pentad_std_q90_qr)], all.x = TRUE, by = c("KG_class_2", "date"))
exeves_qr[, evap_event := FALSE]
exeves_qr[, value_above_low_thres := FALSE]
exeves_qr[, extreme := FALSE]
exeves_qr[std_value > pentad_median_qr, value_above_low_thres := TRUE]
exeves_qr[std_value > pentad_std_q90_qr, extreme := TRUE]
exeves_qr[, above_low_thres_id := rleid(value_above_low_thres)]
exeves_qr[, extreme_qr_id := rleid(extreme), .(KG_class_2)]

exeves_qr[extreme == TRUE, evap_event := TRUE, .(KG_class_2, above_low_thres_id)]
above_low_thres_ids_with_extreme <- exeves_qr[extreme == TRUE, above_low_thres_id]
exeves_qr[above_low_thres_id %in% above_low_thres_ids_with_extreme, evap_event := TRUE]
exeves_qr[, event_qr_id := rleid(evap_event), .(KG_class_2)]
exeves_qr[evap_event != TRUE, event_qr_id := NA]
exeves_qr[extreme != TRUE, extreme_qr_id := NA]

### Mean - Quantile 0.75 definition
exeves_75 <- merge(evap, pentads[, .(KG_class_2, date, std_value, pentad_median_qr, 
                                  pentad_std_q75, pentad_std_q80, pentad_std_q90, pentad_std_q90_qr)], 
                all.x = TRUE, by = c("KG_class_2", "date"))

exeves_75[, evap_event := FALSE]
exeves_75[, value_above_low_thres := FALSE]
exeves_75[, extreme := FALSE]
exeves_75[std_value > pentad_std_q75, value_above_low_thres := TRUE]
exeves_75[std_value > pentad_std_q90, extreme := TRUE]
exeves_75[, above_low_thres_id := rleid(value_above_low_thres)]
exeves_75[, extreme_id := rleid(extreme), .(KG_class_2)]

exeves_75[extreme == TRUE, evap_event := TRUE, .(KG_class_2, above_low_thres_id)] 
above_low_thres_ids_with_extreme <- exeves_75[extreme == TRUE, above_low_thres_id]
exeves_75[above_low_thres_id %in% above_low_thres_ids_with_extreme, evap_event := TRUE]
exeves_75[, event_75_id := rleid(evap_event), .(KG_class_2)]
exeves_75[evap_event != TRUE, event_75_id := NA]
exeves_75[extreme != TRUE, extreme_id := NA]

### Quantile 0.80 definition
exeves_80 <- merge(evap, pentads[, .(KG_class_2, date, std_value, pentad_median_qr, 
                                  pentad_std_q80)], 
                all.x = TRUE, by = c("KG_class_2", "date"))

exeves_80[, evap_event := FALSE]
exeves_80[std_value > pentad_std_q80, evap_event := TRUE]
exeves_80[, event_80_id := rleid(evap_event), .(KG_class_2)]
exeves_80[evap_event != TRUE, event_80_id := NA]

exeves <- merge(exeves[, .(KG_class_2, date, season, period, value, std_value, event_id, extreme_id)], 
      exeves_qr[, .(KG_class_2, date, event_qr_id, extreme_qr_id)], by = c('KG_class_2', 'date'))
      
exeves <- merge(exeves, 
      exeves_75[, .(KG_class_2, date, event_75_id)], by = c('KG_class_2', 'date'))
      
exeves <- merge(exeves, 
      exeves_80[, .(KG_class_2, date, event_80_id)], by = c('KG_class_2', 'date'))

saveRDS(pentads, paste0(PATH_OUTPUT_DATA, 'pentads_std_', region, '.rds'))
saveRDS(exeves, paste0(PATH_OUTPUT_DATA, 'exeves_std_', region, '.rds'))

rm(evap); rm(exeves); gc()

# Precipitation
prec <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_prec.rds'))

prec <- evap_grid[prec, on = .(lon, lat)][, lon := NULL][, lat := NULL]
prec[, q90 := quantile(value, 0.9), KG_class_2]
prec[value > q90, extreme_prec := TRUE][, q90 := NULL]

saveRDS(prec, paste0(PATH_OUTPUT_DATA, region, '_prec_grid.rds'))

# Radiation
lwrad <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_lwrad.rds'))
swrad <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_swrad.rds'))

lwrad <- evap_grid[lwrad, on = .(lon, lat)][, lon := NULL][, lat := NULL]
swrad <- evap_grid[swrad, on = .(lon, lat)][, lon := NULL][, lat := NULL]

lwrad <- lwrad[pentads[, .(date, pentad, KG_class_2)], on = .(date, KG_class_2)]
swrad <- swrad[pentads[, .(date, pentad, KG_class_2)], on = .(date, KG_class_2)]

lwrad[, std_value := (value - mean(value)) / sd(value), by = .(pentad, KG_class_2)][, pentad := NULL]
swrad[, std_value := (value - mean(value)) / sd(value), by = .(pentad, KG_class_2)][, pentad := NULL]

saveRDS(lwrad, paste0(PATH_OUTPUT_DATA, region, '_lwrad_grid.rds'))
saveRDS(swrad, paste0(PATH_OUTPUT_DATA, region, '_swrad_grid.rds'))


