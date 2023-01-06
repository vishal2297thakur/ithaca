### Determine the regions that have lower observation bias according to the 
### coefficient of variability of the datasets

source('source/blueprint.R')
source('source/geo_functions.R')
source('source/graphics.R')

## Read data 
prec_stats <- readRDS(paste0(path_save_blueprint, "ensemble_prec_stats.rds"))

## Set variables
quantiles <- seq(0.1, 1, 0.1)
bias_thres_cv <- 0.8

## Main estimations
prec_stats_mean <- prec_stats[, lapply(.SD, mean), .SDcols = c('mean', 'sd', 'cv'), by = c('lon', 'lat')]
prec_stats_low_bias <- prec_stats_mean[cv <= bias_thres_cv,  .(lon, lat)]

cv_quantiles <- prec_stats_mean[, quantile(cv, quantiles)]
cv_values_n <- data.table(quantile = quantiles, 
                          cv = cv_quantiles, 
                          size = floor(quantile(1:nrow(prec_stats_mean), quantiles)))

## Save data for further use
saveRDS(prec_stats_mean, paste0(path_save_blueprint, "prec_stats_mean.rds"))
saveRDS(prec_stats_low_bias, paste0(path_save_blueprint, "prec_stats_low_bias.rds"))

## Plot results
to_plot <- prec_stats_low_bias
p00 <- ggplot(to_plot) +
  geom_raster(aes(x = lon, y = lat, fill = "")) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(to_plot$lon), max(to_plot$lon)), 
                  ylim = c(min(to_plot$lat), max(to_plot$lat))) +  
  labs(x = "", y = "", fill = 'Low bias') +
  scale_x_continuous(expand = c(0.015, 0.015)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.grid = element_line(color = "grey")) 

y_labs <- ggplot_build(p00)$layout$panel_params[[1]]$y$get_labels()
x_labs <- ggplot_build(p00)$layout$panel_params[[1]]$x$get_labels()
p01 <- p00 + scale_x_continuous(expand = c(0.015, 0.015), labels = paste0(x_labs, "\u00b0")) +
  scale_y_continuous(expand = c(0.0125, 0.0125),  labels = paste0(y_labs, "\u00b0"))
p01

to_plot <- cv_values_n
ggplot(to_plot, aes(y = size, x = cv)) +
  geom_point() +
  geom_line() +
  labs(y = "Size", x = "CV") +
  theme_light() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.grid = element_line(color = "grey")) 


