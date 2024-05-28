############################################################################
#Spatial datatable with basin, area and geometry 
############################################################################
# Required library 
library(data.table)
library(sf)
library(fst)

SHP_PATH <- "~/shared/data_projects/ithaca/pet_europe/data/europe_basin_shapefile/"
BASIN_AREA_PATH <- "~/shared/data_projects/ithaca/pet_europe/data/"
SAVE_PATH <- "~/shared/data_projects/ithaca/pet_europe/data/"

# List all shapefiles in the folder
shp_files <- list.files(path = SHP_PATH, pattern = "\\.shp$", full.names = TRUE)

# function to add basin column in shapefiles 
shp_add_basin <- function(shp_file) {
  sf_data <- st_read(shp_file)
  
  # Extract the basin ID from the shapefile name
  basin_id <- gsub("mask_", "", tools::file_path_sans_ext(basename(shp_file)))
  
  # Replace the "DN" column with the basin ID
  sf_data$DN <- basin_id
  
  return(sf_data)
}

# Read the shapefiles into a list of sf objects
shpfile_list <- lapply(shp_files, shp_add_basin)

# creating the datatable with sf objects 
shp_dt <- do.call(rbind, shpfile_list)
shp_dt <- as.data.table(shp_dt)
setnames(shp_dt,"DN","basin")

basin_list <- unique(shp_dt$basin)

#creating basin area datatable
basin <- fread(paste0(BASIN_AREA_PATH,"LUT_merged_v3_hicam_eu_areasorted.txt"))
basin_sort <- basin[,c("id", "area")]
basin_sort <- basin_sort[id %in% basin_list,]
setnames(basin_sort, "id", "basin")
basin_sort <- basin_sort[, basin := as.factor(basin)]

# Merging area and shapefile datatable 
combined_dt <- merge.data.table(shp_dt,basin_sort, by = "basin")
setorder(combined_dt, -area)

saveRDS(combined_dt,paste0(SAVE_PATH, "shapefile_area_datatable.rds"))

