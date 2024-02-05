# lon lat KG tables
source("source/uncertainty_prec.R")

## Data
prec_month <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_month.rds"))

prec_years <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_years.rds"))

prec_masks <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC_SPATIAL,
                             "pRecipe_masks.rds"))

prec_masks <- prec_masks[, .(lon, lat, KG_class_1_name, land_cover_short_class,
                             biome_short_class, elev_class)]

###
prec_month <- merge(prec_month, prec_masks, by = c("lon", "lat"))

prec_month <- dcast(prec_month, lon + lat + KG_class_1_name +
                      land_cover_short_class + biome_short_class +
                      elev_class ~ dataset,
        value.var = "t_prec")

###
prec_years <- merge(prec_years, prec_masks, by = c("lon", "lat"))

prec_years <- dcast(prec_years, lon + lat + KG_class_1_name +
                      land_cover_short_class + biome_short_class +
                      elev_class ~ dataset,
                    value.var = "t_prec")

fwrite(prec_month, file = "~/shared/t_metric_summary_month.csv")
fwrite(prec_years, file = "~/shared/t_metric_summary_years.csv")
