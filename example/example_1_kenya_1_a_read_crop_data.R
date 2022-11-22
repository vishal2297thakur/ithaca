source('source/main.R')
source('source/example_kenya_source.R')

### Read data

fname_var1_1 <- list.files(path = paths_var1, full.names = T, pattern = "era5_tp*") 
fname_var1_2 <- list.files(path = paths_var1, full.names = T, pattern = "terraclimate_tp*") 

fname_var2_1 <- list.files(path = paths_var2, full.names = T, pattern = "era5_e_*") 
fname_var2_2 <- list.files(path = paths_var2, full.names = T, pattern = "terraclimate_e_*") 

## Reading the data and extracting the data for the specified region crop_box for a period of 60 years (12x60 months)
## Variable 1
# Read data
var1_1 <- brick(fname_var1_1)
var1_2 <- brick(fname_var1_2)

# Crop data (Kenya) and filter to time
var1_1_crop <- crop(var1_1, crop_box)
var1_1_crop <- var1_1_crop[[13:732]]

var1_2_crop <- crop(var1_2, crop_box)
var1_2_crop <- var1_2_crop[[25:744]]

# Remove global data
rm(var1_1)
rm(var1_2)

## Variable 2
# Read data
var2_1 <- brick(fname_var2_1)
var2_2 <- brick(fname_var2_2)

# Crop data (Kenya) and filter to time
var2_1_crop <- crop(var2_1, crop_box)
var2_1_crop <- var2_1_crop[[13:732]]

var2_2_crop <- crop(var2_2, crop_box)
var2_2_crop <- var2_2_crop[[25:744]]

# Remove global data
rm(var2_1)
rm(var2_2)

