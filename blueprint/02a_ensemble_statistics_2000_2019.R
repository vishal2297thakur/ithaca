### Estimation of ensemble statistics

install.packages("gtools")
library(gtools)

source('source/blueprint.R')
source('source/geo_functions.R')
source('source/graphics.R')

## Read data 
prec_2000_2019 <- lapply(prec_fnames_2000_2019_kenya, brick)
names(prec_2000_2019) <- prec_fnames_short_2000_2019_kenya 

#prec_2000_2019 <- prec_2000_2019[c('cru-ts', 'era5', 'em-earth', 'gpcc', 'precl')] #pilot study

## Set functions
estimate_q25 <- function(x) {as.numeric(quantile(x, 0.25, na.rm = TRUE))}
estimate_q75 <- function(x) {as.numeric(quantile(x, 0.75, na.rm = TRUE))}

## Set variables
period_months_dates <- seq(period_start, by = "month", length.out = period_months)

## Main estimations
# Total
prec_mean <- lapply(prec_2000_2019, calc, fun = mean)
prec_ens_mean_mean <- calc(stack(prec_mean), fun = mean, na.rm = T)
prec_ens_stats <- data.table(rasterToPoints(prec_ens_mean_mean))
colnames(prec_ens_stats) <- c('lon', 'lat', 'ens_mean_mean')
prec_ens_stats[, ens_mean_mean := round(ens_mean_mean, 0)]

prec_sd <- lapply(prec_2000_2019, calc, fun = sd)
prec_ens_sd_mean <- calc(stack(prec_sd), fun = mean, na.rm = T)
prec_ens_sd_mean_dt <- data.table(rasterToPoints(prec_ens_sd_mean))
colnames(prec_ens_sd_mean_dt) <- c('lon', 'lat', 'ens_sd_mean')
prec_ens_sd_mean_dt[, ens_sd_mean := round(ens_sd_mean, 0)]
prec_ens_stats <- merge(prec_ens_stats, prec_ens_sd_mean_dt, by = c('lon', 'lat'))

prec_ens_mean_median <- calc(stack(prec_mean), fun = median, na.rm = T)
prec_ens_mean_median_dt <- data.table(rasterToPoints(prec_ens_mean_median))
colnames(prec_ens_mean_median_dt) <- c('lon', 'lat', 'ens_mean_median')
prec_ens_mean_median_dt[, ens_mean_median := round(ens_mean_median, 0)]
prec_ens_stats <- merge(prec_ens_stats, prec_ens_mean_median_dt, by = c('lon', 'lat'))

prec_ens_mean_sd <- calc(stack(prec_mean), fun = sd, na.rm = T)
prec_ens_mean_sd_dt <- data.table(rasterToPoints(prec_ens_mean_sd))
colnames(prec_ens_mean_sd_dt) <- c('lon', 'lat', 'ens_mean_sd')
prec_ens_mean_sd_dt[, ens_mean_sd := round(ens_mean_sd, 0)]
prec_ens_stats <- merge(prec_ens_stats, prec_ens_mean_sd_dt, by = c('lon', 'lat'))

prec_ens_mean_q25 <- calc(stack(prec_mean), fun = estimate_q25)
prec_ens_mean_q25_dt <- data.table(rasterToPoints(prec_ens_mean_q25))
colnames(prec_ens_mean_q25_dt) <- c('lon', 'lat', 'ens_mean_q25')
prec_ens_mean_q25_dt[, ens_mean_q25 := round(ens_mean_q25, 0)]
prec_ens_stats <- merge(prec_ens_stats, prec_ens_mean_q25_dt, by = c('lon', 'lat'))

prec_ens_mean_q75 <- calc(stack(prec_mean), fun = estimate_q75)
prec_ens_mean_q75_dt <- data.table(rasterToPoints(prec_ens_mean_q75))
colnames(prec_ens_mean_q75_dt) <- c('lon', 'lat', 'ens_mean_q75')
prec_ens_mean_q75_dt[, ens_mean_q75 := round(ens_mean_q75, 0)]
prec_ens_stats <- merge(prec_ens_stats, prec_ens_mean_q75_dt, by = c('lon', 'lat'))
prec_ens_mean_q25_dt[, ens_mean := round(ens_mean_q25, 0)]

prec_ens_stats[, std_quant_range := round((ens_mean_q75 - ens_mean_q25) / ens_mean_median, 2)] # Using q75 - q25 as in Sun et al. 2018 paper
prec_ens_stats[, ens_mean_cv := round(ens_mean_sd / ens_mean_mean, 2)]
prec_ens_stats[, quant_ens_cv := ordered(quantcut(ens_mean_cv, 5), 
                                         labels = c('0-0.2', '0.2-0.4', '0.4-0.6', '0.6-0.8', '0.8-1.00'))]
prec_ens_stats[, rel_dataset_agreement := ordered(quantcut(std_quant_range, 5), 
                                                  labels = c('high', 'above average', 'average', 'below average', 'low'))]

prec_ens_stats[, abs_dataset_agreement := ordered(1, labels = "very high")]
prec_ens_stats[std_quant_range > 0.1 & std_quant_range < 0.2, abs_dataset_agreement := ordered(2, labels = "high")]
prec_ens_stats[std_quant_range > 0.2 & std_quant_range < 0.4, abs_dataset_agreement := ordered(3, labels = "above average")]
prec_ens_stats[std_quant_range > 0.4 & std_quant_range < 0.6, abs_dataset_agreement := ordered(4, labels = "average")]
prec_ens_stats[std_quant_range > 0.6 & std_quant_range < 0.8, abs_dataset_agreement := ordered(5, labels = "below average")]
prec_ens_stats[std_quant_range > 0.8 & std_quant_range < 1, abs_dataset_agreement := ordered(6, labels = "low")]
prec_ens_stats[std_quant_range > 1, abs_dataset_agreement := ordered(7, labels = "very low")]

prec_ens_stats[, outlier_dataset := FALSE]
prec_ens_stats[ens_mean_mean / ens_mean_median > 1.2 | ens_mean_mean / ens_mean_median < 0.8, outlier_dataset := TRUE]

# Monthly - Not sure if needed
prec_ens_mean_month <- foreach(month_count = 1:period_months, .packages = 'raster') %dopar% {
  pick_rasters <- sapply(prec_2000_2019, "[[", month_count)
  calc(stack(pick_rasters), fun = mean, na.rm = T)
}
prec_ens_mean_month <- stack(prec_ens_mean_month)
prec_ens_mean_month <- setZ(prec_ens_mean_month, period_months_dates)
names(prec_ens_mean_month) <- as.Date(period_months_dates)
prec_ens_mean_month_dt <- brick_to_dt(prec_ens_mean_month) 
colnames(prec_ens_mean_month_dt) <-  c('lon', 'lat', 'time', 'ens_mean')

prec_ens_mean_sd_month <- foreach(month_count = 1:period_months, .packages = 'raster') %dopar% {
  pick_rasters <- sapply(prec_2000_2019, "[[", month_count)
  calc(stack(pick_rasters), fun = sd, na.rm = T)
}
prec_ens_mean_sd_month <- stack(prec_ens_mean_sd_month)
prec_ens_mean_sd_month <- setZ(prec_ens_mean_sd_month, period_months_dates)
names(prec_ens_mean_sd_month) <- as.Date(period_months_dates)
prec_ens_mean_sd_month_dt <- brick_to_dt(prec_ens_mean_sd_month) 
colnames(prec_ens_mean_sd_month_dt) <-  c('lon', 'lat', 'time', 'ens_mean_sd')

prec_ens_mean_q25_month <- foreach(month_count = 1:period_months, .packages = 'raster') %dopar% {
  pick_rasters <- sapply(prec_2000_2019, "[[", month_count)
  calc(stack(pick_rasters), fun = estimate_q25)
}
prec_ens_mean_q25_month <- stack(prec_ens_mean_q25_month)
prec_ens_mean_q25_month <- setZ(prec_ens_mean_q25_month, period_months_dates)
names(prec_ens_mean_q25_month) <- as.Date(period_months_dates)
prec_ens_mean_q25_month_dt <- brick_to_dt(prec_ens_mean_q25_month) 
colnames(prec_ens_mean_q25_month_dt) <-  c('lon', 'lat', 'time', 'ens_mean_q25')

prec_ens_mean_q75_month <- foreach(month_count = 1:period_months, .packages = 'raster') %dopar% {
  pick_rasters <- sapply(prec_2000_2019, "[[", month_count)
  calc(stack(pick_rasters), fun = estimate_q75)
}
prec_ens_mean_q75_month <- stack(prec_ens_mean_q75_month)
prec_ens_mean_q75_month <- setZ(prec_ens_mean_q75_month, period_months_dates)
names(prec_ens_mean_q75_month) <- as.Date(period_months_dates)
prec_ens_mean_q75_month_dt <- brick_to_dt(prec_ens_mean_q75_month) 
colnames(prec_ens_mean_q75_month_dt) <-  c('lon', 'lat', 'time', 'ens_mean_q75')

prec_ens_stats_month <- merge(prec_ens_mean_month_dt, prec_ens_mean_sd_month_dt, by = c('lon', 'lat', 'time'))
prec_ens_stats_month <- merge(prec_ens_stats_month, prec_ens_mean_q25_month_dt, by = c('lon', 'lat', 'time'))
prec_ens_stats_month <- merge(prec_ens_stats_month, prec_ens_mean_q75_month_dt, by = c('lon', 'lat', 'time'))

## Quick validation
plot(mean(prec_2000_2019$`cru-ts`))
plot(mean(prec_2000_2019$era5))
plot(mean(prec_2000_2019$`em-earth`))
plot(mean(prec_2000_2019$gpcc))
plot(mean(prec_2000_2019$`gpm-imerg`))
plot(mean(prec_ens_mean_month))

## Extra variables
#prec_stats <- readRDS(paste0(path_save_blueprint, "ensemble_prec_stats.rds"))
prec_ens_stats_month[, month := month(time)]
prec_ens_stats_month[, year := year(time)]
prec_ens_stats_month <- prec_ens_stats_month[, .(lon, lat, time, month, year, ens_mean, ens_mean_sd, ens_mean_q25, ens_mean_q75)]
prec_ens_stats_month[, std_quant_range := (ens_mean_q75 - ens_mean_q25) / ens_mean] # Using q75 - q25 as in Sun et al. 2018 paper
prec_ens_stats_month[, ens_mean_cv := ens_mean_sd / ens_mean]
prec_ens_stats_month[, quant_ens_cv := ordered(quantcut(ens_mean_cv, 5), 
                                               labels = c('0-0.2', '0.2-0.4', '0.4-0.6', '0.6-0.8', '0.8-1.00'))]
prec_ens_stats_month[, dataset_agreement := ordered(quantcut(std_quant_range, 5), 
                                                    labels = c('high', 'above average', 'average', 'below average', 'low'))]


prec_ens_stats_month_mean <- prec_ens_stats_month[, lapply(.SD, mean), 
                                                  .SDcols =  c('ens_mean', 'ens_mean_sd', 'ens_mean_cv', 'ens_mean_q25', 'ens_mean_q75', 'std_quant_range'), 
                                                  by = c('lon', 'lat', 'month')]

## Save data for further use
saveRDS(prec_ens_stats, paste0(path_save_blueprint, "prec_ensemble_stats.rds"))
saveRDS(prec_ens_stats_month, paste0(path_save_blueprint, "prec_ensemble_stats_month.rds"))
saveRDS(prec_ens_stats_month_mean, paste0(path_save_blueprint, "prec_ensemble_stats_month_mean.rds"))

## Plot results
to_plot <- prec_ens_stats[, .(value = mean(ens_mean_mean)), .(lon, lat)]
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

