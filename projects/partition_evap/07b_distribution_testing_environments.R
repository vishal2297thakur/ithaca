# distribution test for environments

source('source/partition_evap.R')
## data ----

### Landcover ----
data <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "interannual_variance_land_cover.rds"))
dataset_list <- data[, unique(dataset)]

for (i in 1:(n_datasets_2000_2019-1)){
  print(dataset_list[i])
  dummy1 <- data[dataset %in% dataset_list[i]]
  for (j in (i+1):n_datasets_2000_2019){
    print(dataset_list[j])
    dummy2 <- data[dataset %in% dataset_list[j]]
    dummy_data <- merge(dummy1, dummy2, by = c("land_cover_short_class", "year"))
    dummy_result <- dummy_data[, .(KS_test_p = ks.test(evap_mean.x, evap_mean.y)$p.value), .(land_cover_short_class)]
    dummy_result[, dataset.x := dataset_list[i]]
    dummy_result[, dataset.y := dataset_list[j]]
    if(i == 1 & j == 2){
      data_out <- dummy_result
    }else{
      data_out <- merge(data_out, dummy_result, by = c("land_cover_short_class", "KS_test_p", "dataset.x", "dataset.y"), all = T)
    }
  }
}

saveRDS(data_out, paste0(PATH_SAVE_PARTITION_EVAP, "distribution_test_land_cover.rds"))

### Biomes ----
data <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "interannual_variance_biomes.rds"))

for (i in 1:(n_datasets_2000_2019-1)){
  print(dataset_list[i])
  dummy1 <- data[dataset %in% dataset_list[i]]
  for (j in (i+1):n_datasets_2000_2019){
    print(dataset_list[j])
    dummy2 <- data[dataset %in% dataset_list[j]]
    dummy_data <- merge(dummy1, dummy2, by = c("biome_short_class", "year"))
    dummy_result <- dummy_data[, .(KS_test_p = ks.test(evap_mean.x, evap_mean.y)$p.value), .(biome_short_class)]
    dummy_result[, dataset.x := dataset_list[i]]
    dummy_result[, dataset.y := dataset_list[j]]
    if(i == 1 & j == 2){
      data_out <- dummy_result
    }else{
      data_out <- merge(data_out, dummy_result, by = c("biome_short_class", "KS_test_p", "dataset.x", "dataset.y"), all = T)
    }
  }
}

saveRDS(data_out, paste0(PATH_SAVE_PARTITION_EVAP, "distribution_test_biomes.rds"))



### IPCC ----
data <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "interannual_variance_ipcc.rds"))

for (i in 1:(n_datasets_2000_2019-1)){
  print(dataset_list[i])
  dummy1 <- data[dataset %in% dataset_list[i]]
  for (j in (i+1):n_datasets_2000_2019){
    print(dataset_list[j])
    dummy2 <- data[dataset %in% dataset_list[j]]
    dummy_data <- merge(dummy1, dummy2, by = c("IPCC_ref_region", "year"))
    dummy_result <- dummy_data[, .(KS_test_p = ks.test(evap_mean.x, evap_mean.y)$p.value), .(IPCC_ref_region)]
    dummy_result[, dataset.x := dataset_list[i]]
    dummy_result[, dataset.y := dataset_list[j]]
    if(i == 1 & j == 2){
      data_out <- dummy_result
    }else{
      data_out <- merge(data_out, dummy_result, by = c("IPCC_ref_region", "KS_test_p", "dataset.x", "dataset.y"), all = T)
    }
  }
}

saveRDS(data_out, paste0(PATH_SAVE_PARTITION_EVAP, "distribution_test_ipcc.rds"))



### Elevation ----
data <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "interannual_variance_elevation.rds"))

for (i in 1:(n_datasets_2000_2019-1)){
  print(dataset_list[i])
  dummy1 <- data[dataset %in% dataset_list[i]]
  for (j in (i+1):n_datasets_2000_2019){
    print(dataset_list[j])
    dummy2 <- data[dataset %in% dataset_list[j]]
    dummy_data <- merge(dummy1, dummy2, by = c("elev_class", "year"))
    dummy_result <- dummy_data[, .(KS_test_p = ks.test(evap_mean.x, evap_mean.y)$p.value), .(elev_class)]
    dummy_result[, dataset.x := dataset_list[i]]
    dummy_result[, dataset.y := dataset_list[j]]
    if(i == 1 & j == 2){
      data_out <- dummy_result
    }else{
      data_out <- merge(data_out, dummy_result, by = c("elev_class", "KS_test_p", "dataset.x", "dataset.y"), all = T)
    }
  }
}

saveRDS(data_out, paste0(PATH_SAVE_PARTITION_EVAP, "distribution_test_elevation.rds"))

### Koeppen-Geiger

data <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "interannual_variance_KG.rds"))

for (i in 1:(n_datasets_2000_2019-1)){
  print(dataset_list[i])
  dummy1 <- data[dataset %in% dataset_list[i]]
  for (j in (i+1):n_datasets_2000_2019){
    print(dataset_list[j])
    dummy2 <- data[dataset %in% dataset_list[j]]
    dummy_data <- merge(dummy1, dummy2, by = c("KG_class_1", "year"))
    dummy_result <- dummy_data[, .(KS_test_p = ks.test(evap_mean.x, evap_mean.y)$p.value), .(KG_class_1)]
    dummy_result[, dataset.x := dataset_list[i]]
    dummy_result[, dataset.y := dataset_list[j]]
    if(i == 1 & j == 2){
      data_out <- dummy_result
    }else{
      data_out <- merge(data_out, dummy_result, by = c("KG_class_1", "KS_test_p", "dataset.x", "dataset.y"), all = T)
    }
  }
}

saveRDS(data_out, paste0(PATH_SAVE_PARTITION_EVAP, "distribution_test_KG.rds"))

