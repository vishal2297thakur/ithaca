#' Landmask
#'
#' Function to mask a data set
#' 
#' @param x a RasterBrick to be masked
#' @param keep_land logical. If TRUE (default) you get land values, else you get ocean values
#' @return a RasterBrick with the respective mask
#' 
#' @example
#' dummie_brick <- brick("~/shared/data/sim/precip/raw/ncep-doe_tp_mm_global_197901_202208_025_monthly.nc")
#' land_brick <- landmask(dummie_brick)
#' ocean_brick <- landmask(dummie_brick, keep_land = FALSE)

landmask <- function(x, keep_land = TRUE){
  lsmask <- raster("~/shared/data_projects/ithaca/misc/landmask.nc")
  inv_mask <- !keep_land
  mask_ext <- extent(lsmask)
  x_ext <- extent(x)
  if (x_ext < mask_ext){
    lsmask <- crop(lsmask, x_ext)
  }
  dummie <- mask(x, lsmask, inverse = inv_mask)
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
