# Create bivariate trend of trend direction
source('source/evap_trend.R')
library(biscale)
library(ggplot2)
library(cowplot)

evap_trend <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "global_grid_evap_area.rds"))  
evap_trend_points <- evap_trend[theil_sen_slope >= 0, .(N_pos =.N), .(lon,lat)]
dummy <- evap_trend[theil_sen_slope < 0, .(N_neg =.N), .(lon,lat)]
evap_trend_points <- merge(evap_trend_points, dummy, by = c("lon", "lat"), all = T)
evap_trend_points[is.na(N_pos), N_pos := 0]
evap_trend_points[is.na(N_neg), N_neg := 0]

evap_trend_points[, N := sum(N_pos, N_neg), .(lon,lat)]
evap_trend_points[, N_frac_pos := N_pos/N*100, .(lon,lat)]
evap_trend_points[, N_frac_neg := N_neg/N*100, .(lon,lat)]


data <- bi_class(evap_trend_points, x = N_frac_pos, y = N_frac_neg, style = "equal", dim = 4)


map <- ggplot(data) +
  geom_tile(aes(fill = bi_class, x = lon, y = lat), show.legend = FALSE) +
  bi_scale_fill(pal = "DkViolet2", dim = 4) +
  labs(
    title = "Global ET trend [mm. y-2]" ) +
  bi_theme()


legend <- bi_legend(pal = "DkViolet2",
                    dim = 4,
                    xlab = " % + trend ",
                    ylab = " % - trend ",
                    size = 8)

finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0, 0, 0.3, 0.3)

finalPlot
