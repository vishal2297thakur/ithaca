# Best data set per Country
source("source/uncertainty_prec.R")

registerDoParallel(cores = N_CORES - 1)

## Data
prec_data <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric.rds"))

prec_masks <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC_SPATIAL,
                             "pRecipe_masks.rds"))

## Analysis
### Prepare mask and merge
prec_masks <- prec_masks[, .(lon, lat, country)]
prec_masks <- prec_masks[complete.cases(prec_masks)]

lonlat_area <- unique(prec_masks[, .(lon, lat)]) %>% .[, val := 1] %>%
  rasterFromXYZ(res = c(0.25, 0.25)) %>% area() %>% tabular() %>%
  .[, .(lon, lat, area = value)]

prec_masks <- merge(prec_masks, lonlat_area, by = c("lon", "lat"))

prec_data <- merge(prec_data, prec_masks, by = c("lon", "lat"))

### Bootstrapping
bootstrap_data <- foreach (idx = 1:10000, .combine = rbind) %dopar% {
  lonlat_sample <- split(prec_masks, by = "country")
  lonlat_sample <- lapply(lonlat_sample, function(x) {
    MIN_N <- nrow(x)
    dummie <- x[, .SD[sample(.N, ifelse(MIN_N%/%10 == 0, 1, MIN_N%/%10))],
                by = country]
    dummie
  })
  lonlat_sample <- rbindlist(lonlat_sample)
  dummie <- prec_data[lonlat_sample[, .(lon, lat)], on = .(lon, lat)]
  dummie[, area_class := sum(area), .(dataset, country)
  ][, area_weights := area/area_class
  ][, weighted_t := t_prec*area_weights]
  dummie <- dummie[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                   .(dataset, country)]
  dummie$loop_idx <- idx
  return(dummie)
}

### Area Weighted Average
prec_data[, area_class := sum(area), .(dataset, country)
          ][, area_weights := area/area_class
            ][, weighted_t := t_prec*area_weights]

prec_data <- prec_data[, .(prec_t = sum(weighted_t, na.rm = TRUE)),
                         .(dataset, country)]

## Save
fwrite(prec_data,
       file = paste0(PATH_SAVE_UNCERTAINTY_PREC_TABLES, "country_ranking.csv"))

saveRDS(bootstrap_data, file = paste0(PATH_SAVE_UNCERTAINTY_PREC,
                                "country_bootstrap.rds"))
