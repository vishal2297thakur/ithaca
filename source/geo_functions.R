#' Lake mask
#'
#' Function to mask the lakes in a data set
#' 
#' @param x a RasterBrick to be masked
#' @return a RasterBrick with the respective mask

lake_mask <- function(x){
  lsmask <- raster("~/shared/data_projects/ithaca/misc/water-bodies-mask_global_025.nc")
  mask_ext <- extent(lsmask)
  x_ext <- extent(x)
  if (x_ext < mask_ext){
    lsmask <- crop(lsmask, x_ext)
  }
  dummie <- mask(x, lsmask)
  return(dummie)
}

# Subset a brick object over space and time

crop_space_time <- function(dataset, start, end, crop_box){
  time_filter <- which(getZ(dataset) >= start & 
                         (getZ(dataset) <= end))
  filtered <- subset(dataset, time_filter)
  cropped <- crop(filtered, crop_box)
  return(cropped)
}

# Transform a brick object to data.table format

brick_to_dt <- function(x){
  x_dt <- as.data.frame(x, xy = TRUE, long = TRUE, na.rm = TRUE)
  x_dt <- as.data.table(x_dt)
  setnames(x_dt, colnames(x_dt)[3], 'time')
  x_dt[, time := as.Date(time)]
  return(x_dt)
}

#' Spatial weights
#'
#' Function to mask a data set
#' 
#' @param x a data.table. dt(lon, lat)
#' @return a data.table. dt(lon, lat, weight)

spatial_weight <- function(x){
  x$value <- 1
  coordinates(x) <- ~ lon + lat
  gridded(x) <- TRUE
  x <- raster(x)
  proj4string(x) <- CRS("+proj=longlat +datum=WGS84")
  dummie <- area(x, na.rm = TRUE, weights = TRUE)
  dummie <- as.data.frame(dummie, xy = TRUE, long = TRUE, na.rm = TRUE)
  dummie <- as.data.table(dummie)
  dummie <- dummie[, .(x, y, value)]
  setnames(dummie, c("x", "y", "value"), c("lon", "lat", "weight"))
  return(dummie)
}
