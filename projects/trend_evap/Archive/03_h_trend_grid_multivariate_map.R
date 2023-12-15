# Create multivariate trend of significant trend direction

source('source/evap_trend.R')
library(devtools)
install_github("jschoeley/tricolore")
library(tricolore)
library(ggplot2)
library(ggtern)

evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_evap_area.rds"))  
evap_trend_points <- evap_trend[trend_direction == "positive", .(N_pos =.N), .(lon,lat)]
dummy <- evap_trend[trend_direction == "negative", .(N_neg =.N), .(lon,lat)]
evap_trend_points <- merge(evap_trend_points, dummy, by = c("lon", "lat"), all = T)
dummy <- evap_trend[trend_direction == "no trend", .(N_no =.N), .(lon,lat)]
evap_trend_points <- merge(evap_trend_points, dummy, by = c("lon", "lat"), all = T)
evap_trend_points[is.na(N_pos), N_pos := 0]
evap_trend_points[is.na(N_neg), N_neg := 0]
evap_trend_points[is.na(N_no), N_no := 0]

evap_trend_points[, N := sum(N_pos, N_neg, N_no), .(lon,lat)]
evap_trend_points[, N_frac_pos := N_pos/N*100, .(lon,lat)]
evap_trend_points[, N_frac_neg := N_neg/N*100, .(lon,lat)]
evap_trend_points[, N_frac_no := N_no/N*100, .(lon,lat)]



tri_trends <- Tricolore(evap_trend_points,
                        p1 = "N_frac_pos", p2 = "N_frac_neg", p3 = "N_frac_no", breaks = 3)

evap_trend_points[, colors:= tri_trends$rgb]


ggplot(evap_trend_points) +
  geom_tile(aes(fill = colors, x = lon, y = lat)) +
  scale_fill_identity()+
  annotation_custom(
    ggplotGrob(tri_trends$key+
                 labs(L = '+ trend', T = '- trend', R = 'no trend'))
    , xmin = -200, xmax = -80, ymin = -90, ymax = 20
  )+
  theme_bw()
