# Partition evaporation to different regional properties and quantify their uncertainty
source('source/partition_evap.R')
source('source/geo_functions.R')

## Data 
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
evap_dataset_means <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_datasets.rds"))
evap_grid <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_volume_grid.rds"))
evap_annual <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_global_annual_mean.rds"))

## Variables
land_cover <- merge(evap_mask[, .(lat, lon, land_cover_short_class)], 
                    evap_grid[, .(lon, lat, area)], by = c("lon", "lat"), all = TRUE)
datasets_land_cover <- merge(land_cover, evap_dataset_means, by = c("lon", "lat"))
datasets_land_cover[, evap_volume_year := area * M2_TO_KM2 * evap_mean * MM_TO_KM
][, evap_mean := NULL] # km3
datasets_land_cover <- datasets_land_cover[complete.cases(datasets_land_cover)]
global_mean <- data.table(Source = "All", Global = evap_annual[, mean(evap_volume, na.rm = T)])
dataset_means <- evap_annual[, .(Global = mean(evap_volume, na.rm = T)), dataset]
dataset_means <- merge(unique(evap_dataset_means[, .(dataset, dataset_type)]), dataset_means, by = 'dataset')
colnames(dataset_means)[1] <- 'Dataset'
dataset_types_means <- dataset_means[, .(Global = mean(Global)), dataset_type]
colnames(dataset_types_means)[1] <- 'Source'
dataset_types_means <- rbind(global_mean, dataset_types_means)

## Analysis
datasets_land_cover[, .(area = sum(area)), .(dataset, dataset_type)] #Antarctica 13.66 million km2
land_cover[, sum(area, na.rm = TRUE) / 
             land_cover[, sum(area, na.rm = TRUE)], land_cover_short_class] 
land_cover[, sum(area, na.rm = TRUE), land_cover_short_class]
dataset_partition_land_cover <- datasets_land_cover[, .(evap_sum = round(sum(evap_volume_year), 0)), 
                                    .(land_cover_short_class, dataset, dataset_type)]


### Mean
partition_land_cover_global <- dcast(dataset_partition_land_cover, . ~ land_cover_short_class, 
                             fun = mean, na.rm = TRUE)
colnames(partition_land_cover_global)[1] <- "Source"
partition_land_cover_dataset_types <- dcast(dataset_partition_land_cover, dataset_type ~ land_cover_short_class, 
                                    fun = mean, na.rm = TRUE)
colnames(partition_land_cover_dataset_types)[1] <- "Source"
partition_land_cover <- rbind(partition_land_cover_global, partition_land_cover_dataset_types)
partition_land_cover$Source[1] <- 'All'
partition_land_cover <- merge(partition_land_cover, dataset_types_means, by = "Source") 
partition_land_cover[, Source := c("All","Ensemble","Hydrologic model","Reanalysis","Remote Sensing")]
partition_land_cover[, 2:7 := lapply(.SD, round, 0), .SDcols = 2:7]

partition_land_cover_datasets <- dcast(dataset_partition_land_cover, dataset ~ land_cover_short_class, fun = mean, na.rm = TRUE)
partition_land_cover_datasets <- merge(evap_dataset_means[, .(dataset = unique(dataset)), dataset_type], partition_land_cover_datasets, by = 'dataset')
colnames(partition_land_cover_datasets)[1] <- c("Dataset")
partition_land_cover_datasets <- merge(partition_land_cover_datasets, dataset_means[, .(Dataset, Global)], by = "Dataset")

### St. Dev
partition_land_cover_global_sd <- dcast(dataset_partition_land_cover, . ~ land_cover_short_class, 
                                fun = sd, na.rm = TRUE)
colnames(partition_land_cover_global_sd)[1] <- "Source"
partition_land_cover_dataset_types_sd <- dcast(dataset_partition_land_cover, dataset_type ~ land_cover_short_class, 
                                       fun = sd, na.rm = TRUE)
colnames(partition_land_cover_dataset_types_sd)[1] <- "Source"
partition_land_cover_sd <- rbind(partition_land_cover_global_sd, partition_land_cover_dataset_types_sd)
partition_land_cover_sd[, Source := c("All","Ensemble","Hydrologic model","Reanalysis","Remote Sensing")]
partition_land_cover_sd$Global <- partition_land_cover_datasets[, sd(Global, na.rm = TRUE)]
partition_land_cover_sd$Global[2:4] <- partition_land_cover_datasets[, sd(Global, na.rm = TRUE), dataset_type]$V1[c(2, 3, 1)]
partition_land_cover_sd[, 2:7 := lapply(.SD, round, 0), .SDcols = 2:7]

## Save data
write.csv(partition_land_cover, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "partition_land_cover_global.csv"))
write.csv(partition_land_cover_sd, paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "partition_land_cover_sd_global.csv"))
write.csv(partition_land_cover_datasets[, -2], paste0(PATH_SAVE_PARTITION_EVAP_TABLES, "partition_land_cover_datasets_global.csv"))
saveRDS(datasets_land_cover[, .(lon, lat, dataset, evap_volume_year)] , paste0(PATH_SAVE_PARTITION_EVAP, "evap_dataset_volume_land_cover.rds"))

## Antarctica
13.82 * 10^6 * c(150, 200) * MM_TO_KM
13.82 * 10^6 * 175 * MM_TO_KM
