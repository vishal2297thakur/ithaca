# lon lat KG tables
source("source/uncertainty_prec.R")

## Data
prec_month <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_month.rds"))

prec_years <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "t_metric_years.rds"))

prec_masks <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC_SPATIAL,
                             "pRecipe_masks.rds"))

prec_masks <- prec_masks[, .(lon, lat, KG_class_1_name)]

###
prec_month <- merge(prec_month, prec_masks, by = c("lon", "lat"))

prec_month <- prec_month[, n_gridcells := .N, KG_class_1_name
                         ][, `:=`(avg_t = mean(t_prec, na.rm = TRUE),
                                  ranks = rank(-t_prec, na.last = "keep")),
                           .(lon, lat)
                           ][ranks <= 3
                             ][, ranks := as.character(ranks)
                               ][ranks == "1", ranks := "first_dataset"
                                 ][ranks == "2", ranks := "second_dataset"
                                   ][ranks == "3", ranks := "third_dataset"]

prec_month <- prec_month[, .(lon, lat, KG_class_1_name, n_gridcells, avg_t,
                             dataset, ranks)] %>%
  dcast(lon + lat + KG_class_1_name + n_gridcells + avg_t ~ ranks,
        value.var = "dataset")

prec_month[, n_gridcells := .N, KG_class_1_name]

###
prec_years <- merge(prec_years, prec_masks, by = c("lon", "lat"))

prec_years <- prec_years[, n_gridcells := .N, KG_class_1_name
                         ][, `:=`(avg_t = mean(t_prec, na.rm = TRUE),
                                  ranks = rank(-t_prec, na.last = "keep")),
                           .(lon, lat)
                           ][ranks <= 3
                             ][, ranks := as.character(ranks)
                               ][ranks == "1", ranks := "first_dataset"
                                 ][ranks == "2", ranks := "second_dataset"
                                   ][ranks == "3", ranks := "third_dataset"]

prec_years <- prec_years[, .(lon, lat, KG_class_1_name, n_gridcells, avg_t,
                             dataset, ranks)] %>%
  dcast(lon + lat + KG_class_1_name + n_gridcells + avg_t ~ ranks,
        value.var = "dataset")

prec_years[, n_gridcells := .N, KG_class_1_name]

fwrite(prec_month, file = "~/shared/t_metric_kg_summary_month.csv")
fwrite(prec_years, file = "~/shared/t_metric_kg_summary_years.csv")
