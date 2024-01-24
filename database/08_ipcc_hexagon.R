# Create hexagon with ipcc reference regions ----

source('source/main.R')
source('source/mask_paths.R')

library(rgeos)

### IPCC
fname_shape <- list.files(path = PATH_MASKS_IPCC, full.names = TRUE, pattern = "reference-regions-v4.shp")
shape_mask <- st_read(paste0(fname_shape[1]))
center <- st_centroid(shape_mask)
center_coords <- st_coordinates(center)
ipcc_data <- as.data.table(st_drop_geometry(shape_mask))
ipcc_data[, x := center_coords[,1]]
ipcc_data[, y := center_coords[,2]]
ipcc_data[, x_r := round(x/10)*10]
ipcc_data[, y_r := round(y/10)*10]

ipcc_data_land <- ipcc_data[Type != "Ocean"]
ipcc_data_land <- ipcc_data_land[!Acronym %in% c("WAN", "EAN")]


### test


ggplot(ipcc_data_land, aes(x = x_r, y = y_r))+
  geom_hex(size = 2)+
  geom_text(aes(x = x_r, y = y_r, label = Acronym))+
  theme_bw()
