### Estimation of monthly precipitation mean, standard deviation (sd), and coefficient of variance
### for the dataset ensemble

source('source/example_kenya.R')
source('source/geo_functions.R')

# Read data 
prec_era5_kenya <- readRDS(paste0(path_save_kenya, "prec_era5.rds"))
prec_terra_kenya <- readRDS(paste0(path_save_kenya, "prec_terra.rds"))

# Variables
period_months_dates <- seq(PERIOD_START, by = "month", length.out = period_months)

## Monthly ensemble mean for two data sources 
# Version 1: Parallel computing
prec_mean_month <- foreach(month_count = 1:period_months) %dopar% {
   calc(stack(prec_era5_kenya[[month_count]], 
             prec_terra_kenya[[month_count]]), 
       fun = mean, 
       na.rm = F)
}

prec_mean_month <- brick(prec_mean_month)
prec_mean_month <- setZ(prec_mean_month, period_months_dates)
names(prec_mean_month) <- as.Date(period_months_dates)

prec_sd_month <- foreach(month_count = 1:period_months) %dopar% {
  calc(stack(prec_era5_kenya[[month_count]], 
             prec_terra_kenya[[month_count]]), 
       fun = sd, 
       na.rm = F)
}

prec_sd_month <- brick(prec_sd_month)
prec_sd_month <- setZ(prec_sd_month, period_months_dates)
names(prec_sd_month) <- as.Date(period_months_dates)

prec_cv_month <- foreach(month_count = 1:period_months) %dopar% {
  prec_sd_month[[month_count]]/prec_mean_month[[month_count]]
}

prec_cv_month <- brick(prec_cv_month)
prec_cv_month <- setZ(prec_cv_month, period_months_dates)
names(prec_cv_month) <- as.Date(period_months_dates)

# Quick validation
plot(mean(prec_era5_kenya))
plot(mean(prec_terra_kenya))
plot(mean(prec_mean_month))
plot(mean(prec_sd_month))
plot(mean(prec_cv_month))

# Transforming to data.table 
prec_mean_month_dt <- brick_to_dt(prec_mean_month) #function stored in geofunctions.R
prec_sd_month_dt <- brick_to_dt(prec_sd_month)
prec_cv_month_dt <- brick_to_dt(prec_cv_month)


## Version 2: Alternative example with stackApply 
## Not sure which of two versions is faster with big rasters

prec_mean_month_alt <- stackApply(x = stack(prec_era5_kenya, prec_terra_kenya), 
                           indices = rep(1:period_months, n_datasets), 
                           fun = mean, 
                           na.rm = F)
prec_mean_month_alt <- setZ(prec_mean_month_alt, period_months_dates)
names(prec_mean_month_alt) <- as.Date(period_months_dates)

prec_mean_month_alt_dt <- brick_to_dt(prec_mean_month_alt)

## Plotting

to_plot <- prec_mean_month_dt[, .(value = mean(value)), .(x, y)]

p00 <- ggplot(to_plot) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(to_plot$x), max(to_plot$x)), 
                  ylim = c(min(to_plot$y), max(to_plot$y))) +  
  labs(x = "Lon", y = "Lat", fill = prec_name_short) +
  scale_fill_gradient2(low ="red", mid = "white", high = "blue", midpoint = 0) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.grid = element_line(color = "grey"))
y_labs <- ggplot_build(p00)$layout$panel_params[[1]]$y$get_labels()
x_labs <- ggplot_build(p00)$layout$panel_params[[1]]$x$get_labels()
p01 <- p00 + scale_x_continuous(expand = c(0, 0), labels = paste0(x_labs, "\u00b0")) +
  scale_y_continuous(expand = c(0.0125, 0.0125),  labels = paste0(y_labs, "\u00b0"))
p01

to_plot <- prec_sd_month_dt[, .(value = mean(value)), .(x, y)]

p00 <- ggplot(to_plot) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(to_plot$x), max(to_plot$x)), 
                  ylim = c(min(to_plot$y), max(to_plot$y))) +  
  labs(x = "Lon", y = "Lat", fill = prec_name_short) +
  scale_fill_gradient2(low ="red", mid = "white", high = "blue", midpoint = 0) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.grid = element_line(color = "grey"))
y_labs <- ggplot_build(p00)$layout$panel_params[[1]]$y$get_labels()
x_labs <- ggplot_build(p00)$layout$panel_params[[1]]$x$get_labels()
p01 <- p00 + scale_x_continuous(expand = c(0, 0), labels = paste0(x_labs, "\u00b0")) +
  scale_y_continuous(expand = c(0.0125, 0.0125),  labels = paste0(y_labs, "\u00b0"))
p01

to_plot <- prec_cv_month_dt[, .(value = mean(value)), .(x, y)]

p00 <- ggplot(to_plot) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(to_plot$x), max(to_plot$x)), 
                  ylim = c(min(to_plot$y), max(to_plot$y))) +  
  labs(x = "Lon", y = "Lat", fill = "cv") +
  scale_fill_gradient2(low ="blue", mid = "white", high = "red", midpoint = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.grid = element_line(color = "grey"))
y_labs <- ggplot_build(p00)$layout$panel_params[[1]]$y$get_labels()
x_labs <- ggplot_build(p00)$layout$panel_params[[1]]$x$get_labels()
p01 <- p00 + scale_x_continuous(expand = c(0, 0), labels = paste0(x_labs, "\u00b0")) +
  scale_y_continuous(expand = c(0.0125, 0.0125),  labels = paste0(y_labs, "\u00b0"))
p01

