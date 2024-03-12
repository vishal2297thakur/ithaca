# Best data set per Country
source("source/uncertainty_prec.R")

registerDoParallel(cores = N_CORES - 1)
## Data
prec_month <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_month.rds"))

prec_years <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_years.rds"))

prec_masks <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC_SPATIAL,
                             "pRecipe_masks.rds"))

## Analysis
### Prepare mask
prec_masks <- prec_masks[, .(lon, lat, country)]
prec_masks <- prec_masks[complete.cases(prec_masks)]

lonlat_area <- unique(prec_masks[, .(lon, lat)]) %>% .[, val := 1] %>%
  rasterFromXYZ(res = c(0.25, 0.25)) %>% area() %>% tabular() %>%
  .[, .(lon, lat, area = value)]

prec_masks <- merge(prec_masks, lonlat_area, by = c("lon", "lat"))

###
prec_month <- merge(prec_month, prec_masks, by = c("lon", "lat"))

prec_years <- merge(prec_years, prec_masks, by = c("lon", "lat"))

###
MC_month <- foreach (idx = 1:10000, .combine = rbind) %dopar% {
  lonlat_sample <- split(prec_masks, by = "country")
  lonlat_sample <- lapply(lonlat_sample, function(x) {
    MIN_N <- nrow(x)
    dummie <- x[, .SD[sample(.N, ifelse(MIN_N%/%10 == 0, 1, MIN_N%/%10))],
                by = country]
    dummie
  })
  lonlat_sample <- rbindlist(lonlat_sample)
  dummie <- prec_month[lonlat_sample[, .(lon, lat)], on = .(lon, lat)]
  dummie[, area_class := sum(area), .(dataset, country)
  ][, area_weights := area/area_class
  ][, weighted_t := t_prec*area_weights]
  dummie <- dummie[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                   .(dataset, country)]
  return(dummie)
}


MC_years <- foreach (idx = 1:10000, .combine = rbind) %dopar% {
  lonlat_sample <- split(prec_masks, by = "country")
  lonlat_sample <- lapply(lonlat_sample, function(x) {
    MIN_N <- nrow(x)
    dummie <- x[, .SD[sample(.N, ifelse(MIN_N%/%10 == 0, 1, MIN_N%/%10))],
                by = country]
    dummie
  })
  lonlat_sample <- rbindlist(lonlat_sample)
  dummie <- prec_years[lonlat_sample[, .(lon, lat)], on = .(lon, lat)]
  dummie[, area_class := sum(area), .(dataset, country)
  ][, area_weights := area/area_class
  ][, weighted_t := t_prec*area_weights]
  dummie <- dummie[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                   .(dataset, country)]
  return(dummie)
}

##
prec_month[, area_class := sum(area), .(dataset, country)
][, area_weights := area/area_class
][, weighted_t := t_prec*area_weights]

prec_month <- prec_month[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                         .(dataset, country)]

prec_years[, area_class := sum(area), .(dataset, country)
           ][, area_weights := area/area_class
             ][, weighted_t := t_prec*area_weights]

prec_years <- prec_years[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                         .(dataset, country)]

## Save
fwrite(prec_month,
       file = paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,"country_month.csv"))

fwrite(prec_years,
       file = paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES,"country_years.csv"))

saveRDS(MC_month, file = paste0(PATH_SAVE_UNCERTAINTY_PREC,
                                "prec_country_month.rds"))

saveRDS(MC_years, file = paste0(PATH_SAVE_UNCERTAINTY_PREC,
                                "prec_country_years.rds"))
