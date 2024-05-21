# Plot showing p-value to complete consensus
## Showing this for all positive, all negative and mixed
# DCI of dataset with increasing p-value

source('source/evap_trend.R')

## Data ----
### Input Data generated in projects/trend_evap/bootstrap/01_b
evap_annual_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "evap_annual_trend_bootstrap.rds"))  

## Function

DCI_val <- function(data, p_value){
  data[slope > p_value , significant_theil_sen := FALSE]
  data[slope <= p_value , significant_theil_sen := TRUE]
  DCI_val <- data[,.(sum(slope/abs(slope)*significant_theil_sen, na.rm = TRUE)/.N),]
  return(DCI_val)
}

###
evap_annual_trend[, rank_p := rank(p)]

evap_annual_trend[, DCI_val := DCI_val(.SD, p)]

ggplot(evap_annual_trend)+
  geom_segment(aes(x = p, y = lower, yend = upper, col = dataset))+
  geom_point(aes(x = p, y = slope))+
  labs(y = "", fill = "", x = "")+
  geom_abline(intercept = 0, slope = 0, col = "black")+
  scale_color_manual(values = cols_data)+
  theme_bw()+
  theme(
    axis.title.y = element_blank())

ggplot(evap_annual_trend)+
  geom_point(aes(x = rank_p, y = slope))+
  labs(y = "", fill = "", x = "")+
  theme_bw()+
  theme(
    axis.title.y = element_blank())