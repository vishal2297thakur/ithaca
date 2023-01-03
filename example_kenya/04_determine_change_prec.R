source('source/main.R')
source('source/masks_source.R')
source('source/example_kenya_source.R')

source('example/example_1_kenya_2_a_ensemble_mean_var1.R')

### Assessing % change in ensemble mean between two 30 years time periods (t1=1960 to 1989 & t2=1990 to 2019)

time1_beg <- as.Date("1960-01-01")
time1_end <- as.Date("1989-12-01")
time2_beg <- as.Date("1990-01-01")
time2_end <- as.Date("2019-12-01")

var1_mean_df_period1 <- var1_mean_df[time >= time1_beg & time <= time1_end, mean(value), .(x,y)]
var1_mean_df_period2 <- var1_mean_df[time >= time2_beg & time <= time2_end, mean(value), .(x,y)]

var1_mean_df_change <- merge(var1_mean_df_period1, var1_mean_df_period2, by = c("x","y"))
setnames(var1_mean_df_change, c('V1.x', 'V1.y'), c('mean_tp_period1', 'mean_tp_period2'))
var1_mean_df_change[, diff_tp := mean_tp_period2 - mean_tp_period1]
var1_mean_df_change[, percent_change_tp := (mean_tp_period2 - mean_tp_period1)/mean_tp_period1*100]


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
