# Creates project paths ----
source("source/main.R")

## Paths ----
### Output ----
PATH_SAVE_PALEO <- paste0(PATH_SAVE, "paleo/")  

PATH_SAVE_PALEO_RAW <- paste0(PATH_SAVE, "paleo/raw/")
PATH_SAVE_PALEO_SPATIAL <- paste0(PATH_SAVE, "paleo/spatial/")
PATH_SAVE_PALEO_FIGURES <- paste0(PATH_SAVE, "paleo/figures/")
PATH_SAVE_PALEO_TABLES <- paste0(PATH_SAVE, "paleo/tables/")
PATH_SAVE_PALEO_FIGURES_SUPP <- paste0(PATH_SAVE, "paleo/figures/SUPPLEMENT/")
PATH_SAVE_PALEO_FIGURES_EXPLORE <- paste0(PATH_SAVE, "paleo/figures/Exploration/")
PATH_SAVE_PALEO_FIGURES_MAIN <- paste0(PATH_SAVE, "paleo/figures/MAIN/")

dir.create(PATH_SAVE_PALEO, showWarnings = FALSE)
dir.create(PATH_SAVE_PALEO_RAW, showWarnings = FALSE)
dir.create(PATH_SAVE_PALEO_SPATIAL, showWarnings = FALSE)
dir.create(PATH_SAVE_PALEO_FIGURES, showWarnings = FALSE)
dir.create(PATH_SAVE_PALEO_TABLES, showWarnings = FALSE)
dir.create(PATH_SAVE_PALEO_FIGURES_SUPP , showWarnings = FALSE)
dir.create(PATH_SAVE_PALEO_FIGURES_EXPLORE , showWarnings = FALSE)
dir.create(PATH_SAVE_PALEO_FIGURES_MAIN , showWarnings = FALSE)

save(PATH_SAVE_PALEO, 
     PATH_SAVE_PALEO_RAW,
     PATH_SAVE_PALEO_SPATIAL,
     PATH_SAVE_PALEO_FIGURES,
     PATH_SAVE_PALEO_TABLES,
     PATH_SAVE_PALEO_FIGURES_SUPP,
     PATH_SAVE_PALEO_FIGURES_EXPLORE,
     PATH_SAVE_PALEO_FIGURES_MAIN,
     file = paste0(PATH_SAVE_PALEO, "paleo_paths.Rdata"))
