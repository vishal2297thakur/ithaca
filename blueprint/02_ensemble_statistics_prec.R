### Estimation of monthly precipitation mean, standard deviation (sd), and coefficient of variance
### for the dataset ensemble
install.packages("gtools")
library(gtools)

source('source/blueprint.R')
source('source/masks.R')
source('source/geo_functions.R')
source('source/graphics.R')

## Read data 
prec_kenya_2000_2019 <- lapply(prec_fnames_2000_2019, brick)
names(prec_kenya_2000_2019) <- prec_fnames_short_2000_2019 

prec_era5_kenya <- brick(paste0(path_save_blueprint, "era5_tp_mm_kenya_200006_201912_025_monthly.nc"))
prec_gpcc_kenya <- brick(paste0(path_save_blueprint, "gpcc_tp_mm_kenya_200006_201912_025_monthly.nc"))
prec_em_kenya <- brick(paste0(path_save_blueprint, "em-earth_tp_mm_kenya_200006_201912_025_monthly.nc"))
prec_gpcp_kenya <- brick(paste0(path_save_blueprint, "gpcp_tp_mm_kenya_200006_201912_025_monthly.nc"))
prec_mswep_kenya <- brick(paste0(path_save_blueprint, "mswep_tp_mm_kenya_200006_201912_025_monthly.nc"))
prec_gpm_kenya <- brick(paste0(path_save_blueprint, "gpm-imerg_tp_mm_kenya_200006_201912_025_monthly.nc"))

## Set variables
period_months_dates <- seq(period_start, by = "month", length.out = period_months)

## Main estimations #test
prec_mean_month <- foreach(dataset_count = 1:length(datasets_kenya)) %dopar% { 
  foreach(month_count = 1:period_months) %dopar% {
  calc(stack(datasets_kenya[[month_count]]
       fun = mean, 
       na.rm = T)
}

prec_mean_month <- foreach(month_count = 1:period_months, .packages = 'raster') %dopar% {
  calc(stack(prec_era5_kenya[[month_count]],
             prec_gpcc_kenya[[month_count]],
             prec_em_kenya[[month_count]],
             prec_gpcp_kenya[[month_count]],
             prec_mswep_kenya[[month_count]],
             prec_gpm_kenya[[month_count]]), 
       fun = mean, 
       na.rm = T)
}

prec_mean_month <- brick(prec_mean_month)
prec_mean_month <- setZ(prec_mean_month, period_months_dates)
names(prec_mean_month) <- as.Date(period_months_dates)

prec_sd_month <- foreach(month_count = 1:period_months) %dopar% {
  calc(stack(prec_era5_kenya[[month_count]],
             prec_gpcc_kenya[[month_count]],
             prec_em_kenya[[month_count]],
             prec_gpcp_kenya[[month_count]],
             prec_mswep_kenya[[month_count]],
             prec_gpm_kenya[[month_count]]), 
       fun = sd, 
       na.rm = T)
}

prec_sd_month <- brick(prec_sd_month)
prec_sd_month <- setZ(prec_sd_month, period_months_dates)
names(prec_sd_month) <- as.Date(period_months_dates)

estimate_q25 <- function(x) {as.numeric(quantile(x, 0.25, na.rm = TRUE))}
prec_q25_month <- foreach(month_count = 1:period_months) %dopar% {
  calc(stack(prec_era5_kenya[[month_count]],
             prec_gpcc_kenya[[month_count]],
             prec_em_kenya[[month_count]],
             prec_gpcp_kenya[[month_count]],
             prec_mswep_kenya[[month_count]],
             prec_gpm_kenya[[month_count]]), 
       fun = estimate_q25)
}

prec_q25_month <- brick(prec_q25_month)
prec_q25_month <- setZ(prec_q25_month, period_months_dates)
names(prec_q25_month) <- as.Date(period_months_dates)

estimate_q75 <- function(x) {as.numeric(quantile(x, 0.75, na.rm = TRUE))}
prec_q75_month <- foreach(month_count = 1:period_months) %dopar% {
  calc(stack(prec_era5_kenya[[month_count]],
             prec_gpcc_kenya[[month_count]],
             prec_em_kenya[[month_count]],
             prec_gpcp_kenya[[month_count]],
             prec_mswep_kenya[[month_count]],
             prec_gpm_kenya[[month_count]]), 
       fun = estimate_q75)
}

prec_q75_month <- brick(prec_q75_month)
prec_q75_month <- setZ(prec_q75_month, period_months_dates)
names(prec_q75_month) <- as.Date(period_months_dates)

## Quick validation
plot(mean(prec_era5_kenya))
plot(mean(prec_gpcc_kenya))
plot(mean(prec_em_kenya))
plot(mean(prec_gpcp_kenya))
plot(mean(prec_mswep_kenya))
plot(mean(prec_gpm_kenya))
plot(mean(prec_mean_month))
plot(mean(prec_sd_month))
plot(mean(prec_q25_month))
plot(mean(prec_q75_month))

## Transform to data.table 
prec_mean_month_dt <- brick_to_dt(prec_mean_month) 
prec_sd_month_dt <- brick_to_dt(prec_sd_month)
prec_q25_month_dt <- brick_to_dt(prec_q25_month)
prec_q75_month_dt <- brick_to_dt(prec_q75_month)

prec_stats <- merge(prec_mean_month_dt, prec_sd_month_dt, by = c('x', 'y', 'time'))
prec_stats <- merge(prec_stats, prec_q25_month_dt, by = c('x', 'y', 'time'))
prec_stats <- merge(prec_stats, prec_q75_month_dt, by = c('x', 'y', 'time'))
colnames(prec_stats) <- c('lon', 'lat', 'time', 'ens_mean', 'ens_sd', 'ens_q25', 'ens_q75')

## Extra variables
#prec_stats <- readRDS(paste0(path_save_blueprint, "ensemble_prec_stats.rds"))
prec_stats[, month := month(time)]
prec_stats[, year := year(time)]
prec_stats <- prec_stats[, .(lon, lat, time, month, year, ens_mean, ens_sd, ens_q25, ens_q75)]
prec_stats[, std_quant_range := (ens_q75 - ens_q25) / ens_mean] # Using q75 - q25 as in Sun et al. 2018 paper
prec_stats[, ens_cv := ens_sd / ens_mean]
prec_stats[, quant_cv := ordered(quantcut(ens_cv, 5), 
                                 labels = c('0-0.2', '0.2-0.4', '0.4-0.6', '0.6-0.8', '0.8-1.00'))]
prec_stats[, dataset_agreement := ordered(quantcut(std_quant_range, 5), 
                                          labels = c('high', 'above average', 'average', 'below average', 'low'))]

prec_stats_mean <- prec_stats[, lapply(.SD, mean), 
                              .SDcols = c('ens_mean', 'ens_sd', 'ens_cv', 'ens_q25', 'ens_q75', 'std_quant_range'), 
                              by = c('lon', 'lat')]
prec_stats_mean[, quant_cv := ordered(quantcut(ens_cv, 5), 
                                 labels = c('0-0.2', '0.2-0.4', '0.4-0.6', '0.6-0.8', '0.8-1.00'))]
prec_stats_mean[, dataset_agreement := ordered(quantcut(std_quant_range, 5), 
                                          labels = c('high', 'above average', 'average', 'below average', 'low'))]
prec_stats_mean_month <- prec_stats[, lapply(.SD, mean), 
                                    .SDcols =  c('ens_mean', 'ens_sd', 'ens_cv', 'ens_q25', 'ens_q75', 'std_quant_range'), 
                                    by = c('lon', 'lat', 'month')]

## Save data for further use
saveRDS(prec_stats, paste0(path_save_blueprint, "ensemble_prec_stats.rds"))
saveRDS(prec_stats_mean, paste0(path_save_blueprint, "prec_stats_mean.rds"))
saveRDS(prec_stats_mean_month, paste0(path_save_blueprint, "prec_stats_mean_month.rds"))

## Plot results
to_plot <- prec_stats[, .(value = mean(ens_mean)), .(lon, lat)]
p00 <- ggplot(to_plot) +
  geom_raster(aes(x = lon, y = lat, fill = value)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(to_plot$lon), max(to_plot$lon)), 
                  ylim = c(min(to_plot$lat), max(to_plot$lat))) +  
  labs(x = "", y = "", fill = prec_name_short) +
  scale_fill_gradient2(low = main_cols[3], 
                       mid = "white", 
                       high = "dark blue", 
                       midpoint = 0) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.grid = element_line(color = "grey"))
y_labs <- ggplot_build(p00)$layout$panel_params[[1]]$y$get_labels()
x_labs <- ggplot_build(p00)$layout$panel_params[[1]]$x$get_labels()
p01 <- p00 + scale_x_continuous(expand = c(0, 0), labels = paste0(x_labs, "\u00b0")) +
  scale_y_continuous(expand = c(0.0125, 0.0125),  labels = paste0(y_labs, "\u00b0"))
p01

to_plot <- prec_stats[, .(value = mean(sd)), .(lon, lat)]
p00 <- ggplot(to_plot) +
  geom_raster(aes(x = lon, y = lat, fill = value)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(to_plot$lon), max(to_plot$lon)), 
                  ylim = c(min(to_plot$lat), max(to_plot$lat))) +  
  labs(x = "", y = "", fill = prec_name_short) +
  scale_fill_gradient2(low = main_cols[3], 
                       mid = "white", 
                       high = "dark blue", 
                       midpoint = 0) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.grid = element_line(color = "grey"))
y_labs <- ggplot_build(p00)$layout$panel_params[[1]]$y$get_labels()
x_labs <- ggplot_build(p00)$layout$panel_params[[1]]$x$get_labels()
p01 <- p00 + scale_x_continuous(expand = c(0, 0), labels = paste0(x_labs, "\u00b0")) +
  scale_y_continuous(expand = c(0.0125, 0.0125),  labels = paste0(y_labs, "\u00b0"))
p01

to_plot <- prec_stats[, .(value = mean(cv)), .(lon, lat)]
p00 <- ggplot(to_plot) +
  geom_raster(aes(x = lon, y = lat, fill = value)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(to_plot$lon), max(to_plot$lon)), 
                  ylim = c(min(to_plot$lat), max(to_plot$lat))) +  
  labs(x = "", y = "", fill = "CV") +
  scale_fill_gradient2(low = main_cols[1], 
                       mid = "white", 
                       high = main_cols[3], 
                       midpoint = 0.6) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        panel.grid = element_line(color = "grey"))
y_labs <- ggplot_build(p00)$layout$panel_params[[1]]$y$get_labels()
x_labs <- ggplot_build(p00)$layout$panel_params[[1]]$x$get_labels()
p01 <- p00 + scale_x_continuous(expand = c(0, 0), labels = paste0(x_labs, "\u00b0")) +
  scale_y_continuous(expand = c(0.0125, 0.0125),  labels = paste0(y_labs, "\u00b0"))
p01

