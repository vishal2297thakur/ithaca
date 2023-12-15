# Assemble trend grid data ----
source('projects/trend_evap/00_initialize.R')
source('source/evap_trend.R')
source('source/geo_functions.R')

## Assemble ----
for (dataset_num in EVAP_GLOBAL_DATASETS){
  dummy_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "evap_dataset_",dataset_num,"_trend.rds"))  
  if(dataset_num != EVAP_GLOBAL_DATASETS[1]){
    evap_trend <- rbind( evap_trend, dummy_trend)
  }else{
    evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "evap_dataset_",dataset_num,"_trend.rds"))  
  }
}


## Data ----
evap_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets.rds"))
evap_datasets[, year := as.numeric(as.character(year))]

evap_datasets[ year < 2010, time_period := "2000-2009", ]
evap_datasets[ year >= 2010, time_period := "2010-2019", ]

## Calculate and merge grid stats with trend ----
evap_grid_stats_dummy <- evap_datasets[ time_period == "2000-2009", .(mean_evap_dec1 = mean(evap), median_evap_dec1 = median(evap), sd_evap_dec1 =  sd(evap)) , .(dataset, lat, lon)]

evap_trend <- merge(evap_trend, evap_grid_stats_dummy, by = c("dataset", "lon", "lat"))

evap_grid_stats_dummy <- evap_datasets[ time_period == "2010-2019", .(mean_evap_dec2 = mean(evap), median_evap_dec2 = median(evap), sd_evap_dec2 =  sd(evap)) , .(dataset, lat, lon)]

evap_trend <- merge(evap_trend, evap_grid_stats_dummy, by = c("dataset", "lon", "lat"))


## save data----
saveRDS(evap_trend, paste0(PATH_SAVE_EVAP_TREND, "global_grid_evap_trend.rds"))  

# Code check ----
## plot ----
ggplot(evap_trend)+
  geom_point(aes(x = mean_evap_dec1, y = mean_evap_dec2))+
  facet_wrap(~dataset)+
  geom_abline(slope = 1, intercept = 0, col = "red")+
  theme_bw()
