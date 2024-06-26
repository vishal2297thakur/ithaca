# Significant slopes have p-value <= 0.05 derived from bootstrap ----
# plot global trend direction by product ----
source('source/evap_trend.R')

## Data ----
### Input Data generated in projects/trend_evap/bootstrap/01_b
evap_annual_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "evap_annual_trend_bootstrap.rds"))  
evap_annual_trend[p > 0.1 , trend_direction_v2 := "p > 0.1"]
evap_annual_trend[slope > 0 & p <= 0.1 , trend_direction_v2 := "positive p <= 0.1"]
evap_annual_trend[slope > 0 & p < 0.05 , trend_direction_v2 := "positive p < 0.05"]
evap_annual_trend[slope > 0 & p < 0.01 , trend_direction_v2 := "positive p < 0.01"]
evap_annual_trend[slope < 0 & p <= 0.1 , trend_direction_v2 := "negative p <= 0.1"]
evap_annual_trend[slope < 0 & p < 0.05 , trend_direction_v2 := "negative p < 0.05"]
evap_annual_trend[slope < 0 & p < 0.01 , trend_direction_v2 := "negative p < 0.01"]

## Plot trend direction ----
ggplot(evap_annual_trend)+
  geom_bar(aes(x = dataset, y = trend_score, fill = trend_direction), stat = "identity")+
  geom_abline(intercept = 0, slope = 0, col = "black")+
  scale_fill_manual(values = c("negative" = "darkblue", "positive" = "darkred", "no trend" = "gray"))+
  labs(y = "", fill = "Trend direction")+
  theme_bw()+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "global_trend_direction_by_product_bootstrap.png"), 
       width = 12, height = 8)


ggplot(evap_annual_trend)+
  geom_bar(aes(x = dataset, y = trend_score, fill = trend_direction), stat = "identity")+
  geom_segment(aes(x = dataset, y = lower, yend = upper))+
  geom_point(aes(x = dataset, y = slope))+
  geom_abline(intercept = 0, slope = 0, col = "black")+
  scale_fill_manual(values = c("negative" = "darkblue", "positive" = "darkred", "no trend" = "gray"))+
  labs(y = "", fill = "Trend direction")+
  theme_bw()+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggplot(evap_annual_trend)+
  geom_bar(aes(x = dataset, y = 1, fill = trend_direction_v2), stat = "identity")+
  geom_segment(aes(x = dataset, y = lower, yend = upper))+
  geom_point(aes(x = dataset, y = slope))+
  geom_abline(intercept = 0, slope = 0, col = "black")+
  scale_fill_manual(values = c("p > 0.1" = "gray", 
                               "negative p <= 0.1" = "royalblue1", 
                               "positive p < 0.01" = "darkred",
                               "positive p < 0.05" = "firebrick3",
                               "positive p <= 0.1" = "lightcoral"))+
  labs(y = "", fill = "Trend direction")+
  theme_bw()+
  theme(
    axis.title.y = element_blank())+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))


ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES_SUPP, "global_trend_direction_by_product_bootstrap.png"), 
       width = 8, height = 6, dpi = 600)



