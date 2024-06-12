source('source/partition_evap.R')
source('source/geo_functions.R')

evap_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets.rds"))
evap_stats <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_ensemble_stats.rds"))

unique_lon <- unique(evap_datasets$lon)
unique_lat <- unique(evap_datasets$lat)

for (long in 1:length(unique_lon)){
  
  lon_filter <- evap_datasets[evap_datasets$lon %in% unique_lon[long],]
  
  for (lati in 1:length(unique_lat)){
    
    lat_lon_filter <- lon_filter[lon_filter$lat %in% unique_lat[lati],]
  
  dummy_agreement <- 0
  n_nodata <- 0
  
  for (i in 1:(n_datasets_2000_2019-1)){
    for (j in (i+1):n_datasets_2000_2019){
      
      dummy_mat_1 <- lat_lon_filter[lat_lon_filter$dataset %in% EVAP_GLOBAL_DATASETS[i]]
      dummy_mat_2 <- lat_lon_filter[lat_lon_filter$dataset %in% EVAP_GLOBAL_DATASETS[j]]
      
      if (dim(dummy_mat_1)[1]>0 && dim(dummy_mat_2)[1]>0){
        dummy_pval <- ks.test(dummy_mat_1$evap, dummy_mat_2$evap)$p.value
      } else {
        dummy_pval <- 0
        n_nodata <- n_nodata+1
      }
      
      if (dummy_pval>=0.05){
        dummy_agreement <- dummy_agreement+1
      }
      

    }
  }
  
  dummy_agreement_index <- dummy_agreement/((factorial(n_datasets_2000_2019)/
                                               factorial(n_datasets_2000_2019-2)/
                                               factorial(2))-n_nodata)
  if (long==1 && lati==1){
    distribution_agreement <- data.frame(lon=unique_lon[long],
                                         lat=unique_lat[lati],
                                         agreement_index=dummy_agreement_index)
  }else{
    distribution_agreement <- distribution_agreement %>% 
      add_row(lon=unique_lon[long],
              lat=unique_lat[lati],
              agreement_index=dummy_agreement_index)
  }
  }
  print(long)
}

#saveRDS(distribution_agreement, paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_gridwise.rds"))

ggplot(distribution_agreement)+
  geom_raster(aes(x = lon, y = lat, fill = agreement_index))+
  coord_quickmap()+  labs(x="Lon", y="Lat",fill = "agreement_index")+
  scale_fill_gradientn(colours = terrain.colors(7),na.value = "gray50")+
  ylim(-61, 90)


dummy_mat1_subset <- data.frame(lon=evap_stats$lon,lat=evap_stats$lat)
distribution_agreement_reduced <- merge(distribution_agreement,dummy_mat1_subset)
saveRDS(distribution_agreement_reduced, paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_gridwise.rds"))


