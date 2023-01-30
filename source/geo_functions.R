#' Slopes
#'
#' Function to compute linear slope of each raster grid cell
#' 
#' @param x a RasterBrick to be masked
#' @param annual a character string with the desired aggregation function. Suitable options are:
#' \itemize{
#' \item "max"
#' \item "mean"
#' \item "median"
#' \item "min"
#' \item "sum"
#' }
#' @return a RasterLayer with slope values

brick_slopes <- function(dummie_brick, annual = NULL){
  if(!is.null(annual)){
    dummie_table <- as.data.frame(dummie_brick, xy = TRUE, long = TRUE,
                                  na.rm = TRUE) %>% as.data.table()
    dummie_table <- dummie_table[, value := match.fun(annual)(value),
                                 by = .(x, y, year(Z))][, Z := year(Z)] %>%
      unique() %>% split(by = "y")
  } else {
    dummie_table <- as.data.frame(dummie_brick, xy = TRUE, long = TRUE,
                                  na.rm = TRUE) %>% as.data.table() %>% 
      split(by = "y")
  }
  no_cores <- cores_n - 2
  cluster <- makeCluster(no_cores, type = "PSOCK")
  clusterEvalQ(cluster, library(data.table))
  clusterEvalQ(cluster, library(dplyr))
  dummie_list <- parLapply(cluster, dummie_table, function(dummie_row){
    dummie_row <- dummie_row[, slope := lm(value ~ Z)$coefficients[[2]], by = x
                             ][, .(x, y, slope)] %>% unique()
  })
  stopCluster(cluster)
  dummie_list <- rbindlist(dummie_list)
  coordinates(dummie_list) <- ~ x + y
  gridded(dummie_list) <- TRUE
  dummie <- raster(dummie_list)
  proj4string(dummie) <- CRS("+proj=longlat +datum=WGS84")
  return(dummie)
}

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
crop_time <- function(dataset, start, end){
  time_filter <- which(getZ(dataset) >= start & 
                         (getZ(dataset) <= end))
  cropped <- subset(dataset, time_filter)
  dummie_names <- names(cropped)
  if (!Reduce("|", grepl("^X\\d\\d\\d\\d\\.\\d\\d\\.\\d\\d", 
                         dummie_names))) {
    if (grepl("persiann", nc_out)) {
      dummie_names <- sub("^.", "", dummie_names)
      dummie_names <- as.numeric(dummie_names)
      dummie_Z <- as.Date(dummie_names, origin = "1983-01-01 00:00:00")
    } else if (grepl("gldas-clsm", nc_out)) {
      dummie_names <- sub("^.", "", dummie_names)
      dummie_names <- as.numeric(dummie_names)
      dummie_Z <- as.Date(dummie_names, origin = "1948-01-01 00:00:00")
    }
  } else {
    dummie_Z <- as.Date(dummie_names, format = "X%Y.%m.%d")
  }
  cropped <- setZ(cropped, dummie_Z)
  cropped[cropped <= -9999] <- NA
  return(cropped)
}

crop_space_time <- function(dataset, start, end, crop_box){
  time_filter <- which(getZ(dataset) >= start & 
                         (getZ(dataset) <= end))
  filtered <- subset(dataset, time_filter)
  cropped <- crop(filtered, crop_box)
  dummie_names <- names(cropped)
  if (!Reduce("|", grepl("^X\\d\\d\\d\\d\\.\\d\\d\\.\\d\\d", 
                         dummie_names))) {
    if (grepl("persiann", dataset)) {
      dummie_names <- sub("^.", "", dummie_names)
      dummie_names <- as.numeric(dummie_names)
      dummie_Z <- as.Date(dummie_names, origin = "1983-01-01 00:00:00")
    } else if (grepl("gldas-clsm", dataset)) {
      dummie_names <- sub("^.", "", dummie_names)
      dummie_names <- as.numeric(dummie_names)
      dummie_Z <- as.Date(dummie_names, origin = "1948-01-01 00:00:00")
    }
  } else {
    dummie_Z <- as.Date(dummie_names, format = "X%Y.%m.%d")
  }
  cropped <- setZ(cropped, dummie_Z)
  cropped[cropped <= -9999] <- NA
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

#' Grid area of each cell
#'
#' Function to compute area of each cell in m2
#' 
#' @param x a data.table. dt(lon, lat)
#' @return a data.table. dt(lon, lat, area)

grid_area <- function(x){
  x$value <- 1
  coordinates(x) <- ~ lon + lat
  gridded(x) <- TRUE
  x <- raster(x)
  proj4string(x) <- CRS("+proj=longlat +datum=WGS84")
  dummie <- area(x, na.rm = TRUE)
  dummie <- as.data.frame(dummie, xy = TRUE, long = TRUE, na.rm = TRUE)
  dummie <- as.data.table(dummie)
  dummie <- dummie[, .(x, y, value)][, value := value * 1000000]
  setnames(dummie, c("x", "y", "value"), c("lon", "lat", "area"))
  return(dummie)
}

#' Spatial weights
#'
#' Function to compute weights of each cell in a given region
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

#' Save .nc file
#'
#' Function to save data compatible with pRecipe in .nc file
#'
#' @import ncdf4
#' @param dummie_nc a Raster object to be saved.
#' @param nc_out a character string with the file path for the saved file.
#' @return No return value, called to save a file

save_nc <- function(dummie_nc, nc_out){
  lon <- xFromCol(dummie_nc) %>% round(., 4)
  lat <- yFromRow(dummie_nc) %>% round(., 4)
  time <- getZ(dummie_nc)
  if (is.character(time) | is.numeric(time)) {
    if (is.numeric(time)) {
      time <- as.character(time)
    }
    if (length(time[time == "00"]) >= 1) {
      time <- sub("^00$", "", time)
      time <- time[time != ""]
      time <- as.Date(time)
    } else if (!Reduce("|",grepl("-01", time))) {
      time <- as.numeric(time)
      if (grepl("persiann", nc_out)) {
        dummie_origin <- "1983-01-01 00:00:00"
      } else if (grepl("gldas-", nc_out)) {
        dummie_origin <- "1948-01-01 00:00:00"
      } else {
        dummie_origin <- "1970-01-01 00:00:00"
      }
      time <- as.Date(time, origin = dummie_origin)
    } else {
      time <- as.Date(time)
    }
  }
  tp <- getValues(dummie_nc)
  tp[is.na(tp)] <- -9999
  deflon <- ncdim_def("lon", vals = lon, longname = "longitude",
                      units = "degrees_east")
  deflat <- ncdim_def("lat", vals = lat, longname = "latitude",
                      units = "degrees_north")
  deftime <- ncdim_def("time", vals = as.numeric(time), longname = "time",
                       units = "days since 1970-01-01 00:00:0.0",
                       calendar = "standard",
                       unlim = TRUE)
  deftp <- ncvar_def(name = "tp", units = "mm", 
                     list(deflon, deflat, deftime), 
                     missval = -9999,
                     compression = 4,
                     longname = "Total monthly precipitation",
                     prec = "float")
  ncoutput <- nc_create(nc_out, list(deftp), force_v4 = TRUE, verbose = FALSE)
  ncvar_put(ncoutput, deftp, tp)
  ncatt_put(ncoutput,"lon","axis","X") 
  ncatt_put(ncoutput,"lat","axis","Y")
  ncatt_put(ncoutput,"time","axis","T")
  nc_close(ncoutput)
}
