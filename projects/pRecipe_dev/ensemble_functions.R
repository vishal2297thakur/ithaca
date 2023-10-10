# Ideas for ensemble functionalities added to pRecipe

source("source/main.R")

library(pRecipe)
library(data.table)
library(raster)
library(ggplot2)
library(foreach)
library(doParallel)

# Functions

create_ensemble <- function(data_names, fnames) {
  ensemble <- list()
  dataset_n <- length(fnames)
  for(dataset_index in 1:dataset_n){
    ensemble[[dataset_index]] <- brick(fnames[[dataset_index]])    
  }
  names(ensemble) <- data_names
  return(ensemble)
}

common_period <- function(ensemble){
  dataset_n <- length(ensemble)
  dummy_min <- vector()
  dummy_max <- vector()
  for(dataset_index in 1:dataset_n) {
    dummy_min[dataset_index] <- as.character(min(getZ(ensemble[[dataset_index]])))
    dummy_max[dataset_index] <- as.character(max(getZ(ensemble[[dataset_index]])))
  }
  return <- c(max(dummy_min), min(dummy_max))
}

monthly_means <- function(dataset) {
  indices <- format(as.Date(names(dataset), format = "X%Y.%m.%d"), format = "%m")
  indices <- as.numeric(indices)
  dataset_monthly_means <- stackApply(dataset, indices, fun = mean)
  names(dataset_monthly_means) <- month.abb
  return(dataset_monthly_means)
}

# Example

## Data download & region setup
dataset_names <- c("cru-ts", "era5", "em-earth")
#download_data(dataset = dataset_names, domain = 'land', timestep = 'monthly') #kept it for vignette

dataset_fnames <- c(paste0(PATH_PREC_OBS, 'cru-ts_tp_mm_land_190101_202112_025_monthly.nc'),
                    paste0(PATH_PREC_SIM, 'era5_tp_mm_land_195901_202112_025_monthly.nc'),
                    paste0(PATH_PREC_OBS, 'em-earth_tp_mm_land_195001_201912_025_monthly.nc'))
region_coords <- c(PILOT_LON_MIN, PILOT_LON_MAX, PILOT_LAT_MIN, PILOT_LAT_MAX)

## Create an ensemble by subseting based on pilot study area and sharing common period among datasets
ensemble_datasets <- create_ensemble(dataset_names, dataset_fnames)
ensemble_period <- common_period(ensemble_datasets)   

ensemble_datasets_cropped <- list() #to do: make a foreach parallel loop here | create subset_ensemble function?
ensemble_datasets_cropped[[1]] <- subset_data(ensemble_datasets[[1]], box = region_coords, yrs = year(ensemble_period))
ensemble_datasets_cropped[[2]] <- subset_data(ensemble_datasets[[2]], box = region_coords, yrs = year(ensemble_period))
ensemble_datasets_cropped[[3]] <- subset_data(ensemble_datasets[[3]], box = region_coords, yrs = year(ensemble_period))
names(ensemble_datasets_cropped) <- names(ensemble_datasets) #not sure how this works with foreach - order might change

writeRaster(ensemble_datasets_cropped[[1]], filename = paste0(PATH_SAVE, '/precipe_dev/cru_ts.nc'), 
            format = "CDF", overwrite = TRUE) #ERROR  why .nc doesn't work?

writeRaster(ensemble_datasets_cropped[[1]], filename = paste0(PATH_SAVE, '/precipe_dev/cru.grd'), 
            format = "raster", overwrite = TRUE)
writeRaster(ensemble_datasets_cropped[[2]], filename = paste0(PATH_SAVE, '/precipe_dev/era5.grd'), 
            format = "raster", overwrite = TRUE)
writeRaster(ensemble_datasets_cropped[[3]], filename = paste0(PATH_SAVE, '/precipe_dev/em_earth.grd'), 
            format = "raster", overwrite = TRUE) #to do: make the function save_ensemble(ensemble_datasets_cropped) | why .nc doesn't work?

## Load ensemble and estimate annual and monthly means
dataset_cropped_fnames <- c(paste0(PATH_SAVE, '/precipe_dev/cru.grd'), 
                            paste0(PATH_SAVE, '/precipe_dev/era5.grd'), 
                            paste0(PATH_SAVE, '/precipe_dev/em_earth.grd'))
ensemble_datasets_cropped <- create_ensemble(dataset_names, dataset_cropped_fnames)

ensemble_means <- lapply(datasets_cropped_nc, calc, fun = mean, na.rm = TRUE) #to do: parallelize for any function
plot(ensemble_means$`cru-ts`)
plot(ensemble_means$era5/ensemble_means$`em-earth`)

ensemble_monthly_means <- lapply(ensemble_datasets_cropped, monthly_means)
plot(ensemble_monthly_means$`cru-ts`)
plot(ensemble_monthly_means$era5/ensemble_monthly_means$`em-earth`)

## Compare single year monthly differences
ensemble_dt <- rbindlist(lapply(ensemble_datasets_cropped, fldmean), 
                         idcol = "dataset")
check_year <- 1973     

ggplot() +
  geom_line(data = ensemble_dt, 
            aes(x = factor(month(date)), y = value, group = year(date)), alpha = 0.3) +
  geom_line(data = ensemble_dt[year(date) == check_year],
            aes(x = factor(month(date)), y = value, group = year(date)), col = 'red') +
  facet_wrap(~dataset) +
  theme_minimal()

ggplot() +
  geom_boxplot(data = ensemble_dt, 
               aes(x = factor(month(date)), y = value, fill = dataset)) +
  geom_line(data = ensemble_dt[year(date) == check_year],
            aes(x = factor(month(date)), y = value, col = dataset, group = dataset)) +
  theme_minimal()

ensemble_annual_dt <- ensemble_dt[, .(value = sum(value)), .(year(date), dataset)]
ensemble_annual_dt[ensemble_annual_dt[, .I[value == max(value)], by = dataset]$V1]
ensemble_dt[ensemble_dt[, .I[value == min(value)], by = .(dataset, month(date))]$V1]

## Estimate ensemble statistics
ensemble_stack <- stack(ensemble_datasets_cropped[[1]], ensemble_datasets_cropped[[2]], ensemble_datasets_cropped[[3]]) 
dates <- names(ensemble_datasets_cropped[[1]])
ensemble_mean <- stackApply(ensemble_stack, indices = dates, fun = mean)
ensemble_median <- stackApply(ensemble_stack, indices = dates, fun = median) #takes too much time
ensemble_sd <-stackApply(ensemble_stack, indices = dates, fun = sd)  #takes too much time
ensemble_q25 <- stackApply(ensemble_stack, indices = dates, fun = estimate_q25)  #error
ensemble_q75 <- stackApply(ensemble_stack, indices = dates, fun = estimate_q75)  #error

names(ensemble_mean) <- dates
names(ensemble_median) <- dates
names(ensemble_sd) <- dates

ensemble_stats <- list(ensemble_mean = ensemble_mean, 
                       ensemble_median = ensemble_median, 
                       ensemble_sd = ensemble_sd)

plot(ensemble_stats$ensemble_median)
plot(ensemble_stats$ensemble_sd / ensemble_stats$ensemble_mean)

## somspace
dummy <- tabular(ensemble_median)
dummy <- dummy[, c(3, 2, 1, 4)]
inp_som <- sominp(dummy)
my_som <- somspa(inp_som, rlen = 10000, grid = somgrid(6, 6, "hexagonal"))
my_regions <- somregs(my_som, nregions = 18) 
plot(my_som$som)
plot(my_regions, 3:18, 4, 4)
cnet(my_regions, n = 4, thres = 0.5)





plot_ts(my_regions, 1:5)


