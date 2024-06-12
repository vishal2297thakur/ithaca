library(ggplot2)
library(RPostgres)
library(rpostgis)
library(sf)
library(terra)

con <- dbConnect(Postgres(), dbname = 'earth', host = "10.152.183.146", port = 5432,         
                 user = rstudioapi::askForPassword("Database user"),      
                 password = rstudioapi::askForPassword("Database password"))
basin_atlas_feats <- readRDS(paste0('~/shared/data_projects/schemata/data/basin_atlas_feats.rds'))

regions_all <- c('af', 'ar', 'as', 'au', 'eu', 'gr', 'na', 'sa', 'si')
lon <- 0.75 #Example for Pla et al. 2014 reconstruction
lat <- 41.5
basin_level <- 7 #level of the catchment to plot
sample_region <- regions_all[5] # Sample region is Europe

basin_x <- st_read(con, query = paste0("SELECT * FROM basin_boundaries.", 
                                       sample_region, "_all WHERE ST_Within(ST_SetSRID(ST_POINT(", lon, ", ", lat,"), 4326), geom::geometry)"))   

#basin_x <- st_read(con, query = paste0("SELECT * FROM basin_boundaries.global_all WHERE ST_Within(ST_SetSRID(ST_POINT(", lon, ", ", lat,"), 4326), geom::geometry)")) #Using all continents not just EU - too slow

basin <- merge(basin_x, basin_atlas_feats, by = c('pfaf_id', 'hybas_id', 'coast'))

to_plot <- basin[basin$level == basin_level, ]
dem <- pgGetRast(con, c("basin_dem", "eu_dem_15s_grid"), 
                 boundary = st_bbox(to_plot)[c(4, 2, 3, 1)])
dem_df <- as.data.frame(dem, xy = TRUE)
colnames(dem_df) <- c("x", "y", "Elevation")

ggplot() +
  geom_raster(data = dem_df, aes(x = x, y = y, fill = Elevation)) +
  geom_sf(data = to_plot, col = 'red', size = 3, alpha = 0.1) +
  scale_fill_gradient(low = 'grey10', high = 'white' ) + 
  theme_light() +
  theme(axis.title = element_blank())+
  geom_point(aes(x = lon, y = lat), col = 'dark red') 

to_plot

