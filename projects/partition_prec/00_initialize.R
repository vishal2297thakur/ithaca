#Creates project paths

source("source/main.R")
source("database/06_dataset_fnames.R")

## Paths
### Output
PATH_SAVE_PARTITION_PREC <- paste0(PATH_SAVE, "partition_prec/")
PATH_SAVE_PARTITION_PREC_RAW <- paste0(PATH_SAVE, "partition_prec/raw/")
PATH_SAVE_PARTITION_PREC_SPATIAL <- paste0(PATH_SAVE, "partition_prec/spatial/")
PATH_SAVE_PARTITION_PREC_FIGURES <- paste0(PATH_SAVE, "partition_prec/figures/")
PATH_SAVE_PARTITION_PREC_TABLES <- paste0(PATH_SAVE, "partition_prec/tables/")

dir.create(PATH_SAVE_PARTITION_PREC, showWarnings = FALSE)
dir.create(PATH_SAVE_PARTITION_PREC_RAW, showWarnings = FALSE)
dir.create(PATH_SAVE_PARTITION_PREC_SPATIAL, showWarnings = FALSE)
dir.create(PATH_SAVE_PARTITION_PREC_FIGURES, showWarnings = FALSE)
dir.create(PATH_SAVE_PARTITION_PREC_TABLES, showWarnings = FALSE)

save(PATH_SAVE_PARTITION_PREC, 
     PATH_SAVE_PARTITION_PREC_RAW,
     PATH_SAVE_PARTITION_PREC_SPATIAL,
     PATH_SAVE_PARTITION_PREC_FIGURES,
     PATH_SAVE_PARTITION_PREC_TABLES,
     file = paste0(PATH_SAVE_PARTITION_PREC, "paths.Rdata"))
