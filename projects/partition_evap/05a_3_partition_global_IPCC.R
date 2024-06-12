# Partition evaporation to different regional properties and quantify their uncertainty
source('source/partition_evap.R')
source('source/geo_functions.R')

## Data 
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
evap_dataset_means <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_datasets.rds"))
evap_grid <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_volume_grid.rds"))
evap_annual <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_global_annual_mean.rds"))

## Variables
IPCC_class <- merge(evap_mask[, .(lat, lon, IPCC_ref_region)], 
                    evap_grid[, .(lon, lat, area)], by = c("lon", "lat"), all = TRUE)
datasets_IPCC_class <- merge(IPCC_class, evap_dataset_means, by = c("lon", "lat"))
datasets_IPCC_class[, evap_volume_year := area * M2_TO_KM2 * evap_mean * MM_TO_KM
][, evap_mean := NULL] # km3
datasets_IPCC_class <- datasets_IPCC_class[complete.cases(datasets_IPCC_class)]
global_mean <- data.table(Source = "All", Global = evap_annual[, mean(evap_volume, na.rm = T)])
dataset_means <- evap_annual[, .(Global = mean(evap_volume, na.rm = T)), dataset]
dataset_means <- merge(unique(evap_dataset_means[, .(dataset, dataset_type)]), dataset_means, by = 'dataset')
colnames(dataset_means)[1] <- 'Dataset'
dataset_types_means <- dataset_means[, .(Global = mean(Global)), dataset_type]
colnames(dataset_types_means)[1] <- 'Source'
dataset_types_means <- rbind(global_mean, dataset_types_means)

## Analysis
datasets_IPCC_class[, .(area = sum(area)), .(dataset, dataset_type)] #Antarctica 13.66 million km2
IPCC_class[, sum(area, na.rm = TRUE) / 
             IPCC_class[, sum(area, na.rm = TRUE)], IPCC_ref_region] 
IPCC_class[, sum(area, na.rm = TRUE), IPCC_ref_region]
dataset_partition_IPCC_class <- datasets_IPCC_class[, .(evap_sum = round(sum(evap_volume_year), 0)), 
                                                    .(IPCC_ref_region, dataset, dataset_type)]


### Mean
partition_IPCC_class_global <- dcast(dataset_partition_IPCC_class, . ~ IPCC_ref_region, 
                                     fun = mean, na.rm = TRUE)
colnames(partition_IPCC_class_global)[1] <- "Source"
partition_IPCC_class_dataset_types <- dcast(dataset_partition_IPCC_class, dataset_type ~ IPCC_ref_region, 
                                            fun = mean, na.rm = TRUE)
colnames(partition_IPCC_class_dataset_types)[1] <- "Source"
partition_IPCC_class <- rbind(partition_IPCC_class_global, partition_IPCC_class_dataset_types)
partition_IPCC_class$Source[1] <- 'All'
partition_IPCC_class <- merge(partition_IPCC_class, dataset_types_means, by = "Source") # KG classification has NAs and hence underestimates the total sum
partition_IPCC_class[, Source := c("All","Ensemble","Hydrologic model","Reanalysis","Remote Sensing")]
partition_IPCC_class[, 2:7 := lapply(.SD, round, 0), .SDcols = 2:7]

partition_IPCC_class_datasets <- dcast(dataset_partition_IPCC_class, dataset ~ IPCC_ref_region, fun = mean, na.rm = TRUE)
partition_IPCC_class_datasets <- merge(evap_dataset_means[, .(dataset = unique(dataset)), dataset_type], partition_IPCC_class_datasets, by = 'dataset')
colnames(partition_IPCC_class_datasets)[1] <- c("Dataset")
partition_IPCC_class_datasets <- merge(partition_IPCC_class_datasets, dataset_means[, .(Dataset, Global)], by = "Dataset")

### St. Dev
partition_IPCC_class_global_sd <- dcast(dataset_partition_IPCC_class, . ~ IPCC_ref_region, 
                                        fun = sd, na.rm = TRUE)
colnames(partition_IPCC_class_global_sd)[1] <- "Source"
partition_IPCC_class_dataset_types_sd <- dcast(dataset_partition_IPCC_class, dataset_type ~ IPCC_ref_region, 
                                               fun = sd, na.rm = TRUE)
colnames(partition_IPCC_class_dataset_types_sd)[1] <- "Source"
partition_IPCC_class_sd <- rbind(partition_IPCC_class_global_sd, partition_IPCC_class_dataset_types_sd)
partition_IPCC_class_sd[, Source := c("All","Ensemble","Hydrologic model","Reanalysis","Remote Sensing")]
partition_IPCC_class_sd$Global <- partition_IPCC_class_datasets[, sd(Global, na.rm = TRUE)]
partition_IPCC_class_sd$Global[2:4] <- partition_IPCC_class_datasets[, sd(Global, na.rm = TRUE), dataset_type]$V1[c(2, 3, 1)]
partition_IPCC_class_sd[, 2:7 := lapply(.SD, round, 0), .SDcols = 2:7]

## Save data
write.csv(partition_IPCC_class, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "partition_IPCC_class_global.csv"))
write.csv(partition_IPCC_class_sd, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "partition_IPCC_class_sd_global.csv"))
write.csv(partition_IPCC_class_datasets[, -2], paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "partition_IPCC_class_datasets_global.csv"))
saveRDS(datasets_IPCC_class[, .(lon, lat, dataset, evap_volume_year)] , paste0(PATH_SAVE_PARTITION_EVAP, "evap_dataset_volume_IPCC_class.rds"))

## Antarctica
13.82 * 10^6 * c(150, 200) * MM_TO_KM
13.82 * 10^6 * 175 * MM_TO_KM
