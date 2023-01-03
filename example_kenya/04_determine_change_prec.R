source('source/main.R')
source('source/masks.R')
source('source/graphics.R')
source('source/example_kenya.R')

### Assessing % change in ensemble mean between two 30 years time periods (t1=1960 to 1989 & t2=1990 to 2019)

# Read data 
prec_stats <- readRDS(paste0(path_save_kenya, "prec_stats.rds"))
evap_stats <- readRDS(paste0(path_save_kenya, "evap_stats.rds"))
all_stats_low_bias <- readRDS(paste0(path_save_kenya, "all_stats_low_bias.rds"))

# Set variables
prec_mean__period_1 <- prec_stats[time >= PERIOD_1_START & time <= PERIOD_1_END, mean(mean), .(lon, lat)]
prec_mean__period_2 <- prec_stats[time >= PERIOD_2_START & time <= PERIOD_2_END, mean(mean), .(lon, lat)]
prec_low_bias <- all_stats_low_bias[variable == 'prec', .(lon, lat)]

# Merge data
prec_mean_change <- merge(prec_mean__period_1, prec_mean__period_2, by = c("lat", "lon"))
setnames(prec_mean_change, c('V1.x', 'V1.y'), c('mean_period_1', 'mean_period_2'))

# Main estimations
prec_mean_change[, abs_diff := round(mean_period_2 - mean_period_1, 1)]
prec_mean_change[, perc_change := round((mean_period_2 - mean_period_1)/mean_period_1 * 100, 2)]

# Plot results
to_plot <- prec_mean_change[, .(lat, lon, value = perc_change)]
p00 <- ggplot() +
  geom_raster(data = to_plot, aes(x = lon, y = lat, fill = value)) +
  geom_point(data = all_stats_low_bias, aes(x = lon, y = lat)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(to_plot$lon), max(to_plot$lon)), 
                  ylim = c(min(to_plot$lat), max(to_plot$lat))) +  
  labs(x = "", y = "", fill = "Change (%)") +
  scale_fill_gradient2(low = period_cols[3], 
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


### Assessing % change in ensemble mean between two 30 years time periods (t1=1960 to 1989 & t2=1990 to 2019) for each KG class

fname_shape <- list.files(path = masks_dir_KG_beck, full.names = T, pattern = "climate_beck_level1.shp")
shape_mask <- st_read(paste0(fname_shape[1]))
unique(shape_mask$classes)
shape_mask_v <- st_make_valid(shape_mask)
shape_mask_crop <- st_crop(shape_mask_v, crop_box)
shape_mask_r <- rasterize(shape_mask_crop, var1_1_crop[[1]])
shape_mask_df <- shape_mask_r %>% as.data.frame(xy = TRUE, long = TRUE, na.rm = TRUE)
shape_mask_df <- subset(shape_mask_df, select = c('x', 'y', 'value'))
setnames(shape_mask_df, 'value', 'KG_class')
var1_mean_df_change_KG <- merge(var1_mean_df_change, shape_mask_df, by = c('x','y'), all.x = T)

var1_mean_df_change_KG[,percent_change_tp_KG := mean(percent_change_tp), KG_class]
var1_mean_df_change_KG[,diff_tp_KG := mean(diff_tp), KG_class]

var1_mean_df_change_KG[,unique(percent_change_tp_KG), KG_class]
var1_mean_df_change_KG[,unique(diff_tp_KG), KG_class]
