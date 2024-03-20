# Calulate trend and significance for each grid cell for each dataset
source('source/evap_trend.R')
source('source/geo_functions.R')

library("Kendall")
library("RobustLinearReg")

## Data
evap_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets.rds"))
evap_datasets[, year := as.numeric(as.character(year))]
evap_datasets <- evap_datasets[!(dataset == "etmonitor" & year == 2000), ]

## Analysis
for (dataset_num in EVAP_GLOBAL_DATASETS){
  print(dataset_num)
  evap_dataset_sel <- evap_datasets[dataset == dataset_num,]
  evap_dataset_sel_trend <- evap_trends(evap_dataset_sel)
  
  evap_dataset_sel_trend[lm_p_value > 0.05,significant_lm:= FALSE] 
  evap_dataset_sel_trend[lm_p_value <= 0.05,significant_lm:= TRUE] 
  
  evap_dataset_sel_trend[kendall_p_value > 0.05,significant_kendall:= FALSE] 
  evap_dataset_sel_trend[kendall_p_value <= 0.05,significant_kendall:= TRUE] 
  
  evap_dataset_sel_trend[theil_sen_p_value > 0.05,significant_theil_sen:= FALSE] 
  evap_dataset_sel_trend[theil_sen_p_value <= 0.05,significant_theil_sen:= TRUE] 
  
  evap_dataset_sel_trend[siegel_p_value > 0.05,significant_siegel:= FALSE] 
  evap_dataset_sel_trend[siegel_p_value <= 0.05,significant_siegel:= TRUE] 
  saveRDS(evap_dataset_sel_trend, paste0(PATH_SAVE_EVAP_TREND, "evap_dataset_",dataset_num,"_trend.rds"))  
}
