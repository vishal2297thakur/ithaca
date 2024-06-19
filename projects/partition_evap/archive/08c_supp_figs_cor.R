#Supplementary figure: Correlation network : Needs editing

source('source/partition_evap.R')
source('source/graphics.R')

library(corrr)
library(openair)

## Load data
evap_annual <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_global_annual_mean.rds"))
evap_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_datasets.rds"))
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))

## Variables
data_for_cor_temporal <- dcast(evap_annual, year ~ dataset, value.var = 'evap_mean')

taylor_obs <- evap_annual[dataset == 'gpcc', .(year, obs = evap_mean)]
data_for_taylor <- merge(taylor_obs, evap_annual, by = "year")
setnames(data_for_taylor, 'evap_mean', 'mod') 

## Analyses
### Correlation
cor_matrix_temporal <- correlate(data_for_cor_temporal[, -1])
png(filename = paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/cor_network.png"))
network_plot(cor_matrix_temporal, min_cor = -0, legend = 'range', colors = c('lightsteelblue', colset_RdBu_5[5]))
dev.off()

#### Low dataset agreement and spatial correlation
data_for_cor_spatial_agree <- merge(evap_mask[, .(lon, lat, evap_quant_dataset_agreement)], data_for_cor_spatial, by = c('lon', 'lat'))
data_for_cor_spatial_low_agree <- data_for_cor_spatial_agree[evap_quant_dataset_agreement == 'low']
cor_matrix_spatial_low_agree <- correlate(data_for_cor_spatial_low_agree[, -(1:2)])
network_plot(cor_matrix_spatial_low_agree, min_cor = 0, legend = 'range')

### Taylor plot
data_for_taylor
TaylorDiagram(data_for_taylor, group = "dataset") 

taylor_obs <- evap_annual[, .(obs = mean(evap_mean, na.rm = TRUE)), year]
data_for_taylor <- merge(taylor_obs, evap_annual, by = "year")
setnames(data_for_taylor, 'evap_mean', 'mod') 
TaylorDiagram(data_for_taylor, group = "dataset") 
