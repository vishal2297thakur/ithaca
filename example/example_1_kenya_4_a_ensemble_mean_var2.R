source('source/main.R')
source('source/masks_source.R')
source('source/example_kenya_source.R')

source('example/example_1_kenya_1_a_read_crop_data.R')

### Monthly ensemble mean for two data sources version 1 - keeps time info
# place holder
var2_mean <- var2_1_crop

for(mon_mean in 1:720){
  var2_mean[[mon_mean]] <- mean(var2_1_crop[[mon_mean]], var2_2_crop[[mon_mean]], na.rm = F)
}

var2_mean_df <- var2_mean %>% as.data.frame(xy = TRUE, long = TRUE, na.rm = TRUE)
var2_mean_df <- as.data.table(var2_mean_df)
setnames(var2_mean_df, 'Z', 'time')
var2_mean_df[, time := as.Date(time)]

time1 <- unique(var2_mean_df$time)[1]
time2 <- unique(var2_mean_df$time)[200]

## Monthly ensemble mean version 2 -  replaces time with indices, na.rm = T is the default

var2_mean_v2 <- stackApply(x = stack(var2_1_crop, var2_2_crop), indices = rep(1:720, 2), fun = mean, na.rm = F)
var2_mean_v2_df <- var2_mean_v2 %>% as.data.frame(xy = TRUE, long = TRUE, na.rm = TRUE)
var2_mean_v2_df <- as.data.table(var2_mean_v2_df)
setnames(var2_mean_v2_df, 'layer', 'time')
var2_mean_v2_df[, time := as.factor(time)]
var2_mean_v2_df_levels <- as.numeric(substr(levels(var2_mean_v2_df$time),7,9))
new_levels <- seq(as.Date("1960-1-1"), by = "month", length.out = 720)
levels(var2_mean_v2_df$time) <- new_levels[var2_mean_v2_df_levels]
var2_mean_v2_df[, time := as.Date(time)]



