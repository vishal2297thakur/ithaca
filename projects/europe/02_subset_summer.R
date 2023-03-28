source('source/europe.R')

prec_mswep <- brick(paste0(PATH_SAVE_EUROPE_RAW, "/mswep_tp_mm_europe_198001_201912_025_monthly.nc"))
prec_gpcc <- brick(paste0(PATH_SAVE_EUROPE_RAW, "/gpcc_tp_mm_europe_198001_201912_025_monthly.nc"))
SUMMER_MONTHS <- c(6, 7, 8)
WARM_MONTHS <- c(4, 5, 6, 7, 8, 9)

prec_mswep_dt <- brick_to_dt(prec_mswep)
prec_mswep_summer <- prec_mswep_dt[month(time) %in% SUMMER_MONTHS]
prec_mswep_summer[, year := year(time)]
prec_mswep_summer[, grid_cell := as.factor(paste(x, y, sep = "_"))]
prec_mswep_summer[, summer_sum := sum(value), .(grid_cell, year)][, value := NULL][, time := NULL]

my_slopes <- prec_mswep_summer[, {model <- lm(summer_sum ~ year); coef(model)[2]}, by = .(x, y)]

ggplot(my_slopes)+
  geom_point(aes(x, y, col = V1)) +
  scale_color_gradient2(low =  'tomato', mid = 'grey', high = 'steelblue')+
  theme_bw()

prec_gpcc_dt <- brick_to_dt(prec_gpcc)
prec_gpcc_summer <- prec_gpcc_dt[month(time) %in% SUMMER_MONTHS]
prec_gpcc_summer[, year := year(time)]
prec_gpcc_summer[, grid_cell := as.factor(paste(x, y, sep = "_"))]
prec_gpcc_summer[, summer_sum := sum(value), .(grid_cell, year)][, value := NULL][, time := NULL]

my_slopes <- prec_gpcc_summer[, {model <- lm(summer_sum ~ year); coef(model)[2]}, by = .(x, y)]

ggplot(my_slopes)+
  geom_point(aes(x, y, col = V1)) +
  scale_color_gradient2(low =  'tomato', mid = 'grey', high = 'steelblue')+
  theme_bw()
  
