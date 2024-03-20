source('source/main.R')
library(pRecipe)

dataset <- readRDS('../../shared/data_projects/ithaca/exeves/kenya_evap.rds')

END_PERIOD_1 <- as.Date("2001-12-31")
END_PERIOD_2 <- as.Date("2022-12-31")

dataset[, pentad := ceiling((yday(date) - leap_year(year(date)) * (yday(date) > 59)) / 5 )]
dataset[, pentad_mean := mean(value), .(lon, lat, pentad)]
dataset[, pentad_anomaly := value - pentad_mean, ]
dataset[, period := 'up_to_2001']
dataset[date > END_PERIOD_1, period := 'after_2001']
dataset_monthly <- dataset[, .(value = mean(value)), .(lon, lat, year = year(date), month = month(date), period)]
dataset_annual <- unique(dataset_monthly[, .(value = mean(value)), .(lon, lat, year, period)])

ggplot(dataset_annual) +
  geom_tile(aes(lon, lat, fill = value)) +
  scale_fill_gradient2(low = "orange", mid = "grey90", high = "darkgreen", midpoint = 2) +
  theme_minimal()

quantile_threshold <- 0.9
dataset[, q90 := quantile(value, quantile_threshold), .(lon, lat)]
dataset[, q90_anomaly := quantile(pentad_anomaly, quantile_threshold), .(lon, lat)]
q90_to_mean <- unique(dataset[, .(value = q90 / mean(value), period), .(lon, lat, date = year(date))])
q90_to_mean_slopes <- trend(q90_to_mean)
mean_slopes <- trend(dataset_annual[, .(lon, lat, date = year, value)])

ggplot(mean_slopes) +
  geom_tile(aes(lon, lat, fill = slope)) +
  scale_fill_gradient2(low = "navyblue", mid = "grey90", high = "darkred", midpoint = 0) +
  theme_minimal()

ggplot(q90_to_mean_slopes) +
  geom_tile(aes(lon, lat, fill = slope)) +
  scale_fill_gradient2(low = "navyblue", mid = "grey90", high = "darkred", midpoint = 0) +
  theme_minimal()

q90_to_mean_diff <- q90_to_mean[, .(value = mean(value)), .(lon, lat, period)]  
q90_to_mean_diff <- q90_to_mean_diff[, .(diff_value = diff(value)), by = .(lon, lat)]

ggplot(q90_to_mean_diff) +
  geom_tile(aes(lon, lat, fill = diff_value)) +
  scale_fill_gradient2(low = "navyblue", mid = "grey90", high = "darkred", midpoint = 0) +
  theme_minimal()

dataset[, extreme := FALSE]
dataset[value > q90, extreme := TRUE]
dataset[, event_id := rleid(extreme), .(lon, lat)]
dataset[extreme == TRUE, .N, .(lon, lat, event_id)]
dataset[extreme == TRUE, consec_days := .N, .(event_id, lon, lat)]
dataset_extremes <- unique(dataset[extreme == TRUE, .(lon, lat, month = month(date), year = year(date), event_id, consec_days)])
dataset_extremes_yr <- dataset_extremes[, .(n_events = .N, 
                                            mean_consec_days = mean(consec_days),
                                            max_consec_days = max(consec_days)),
                                        .(year, lon, lat)]

to_plot <- dataset_extremes[, .N, .(lon, lat, month)]
ggplot(to_plot) +
  geom_tile(aes(lon, lat, fill = N)) +
  scale_fill_gradient2(low = "navyblue", mid = "grey90", high = "darkred", midpoint = 50) +
  facet_wrap(~month) +
  theme_minimal()

dataset[, rel_extreme := FALSE]
dataset[pentad_anomaly > q90_anomaly, rel_extreme := TRUE]
dataset[, rel_event_id := rleid(rel_extreme), .(lon, lat)]
dataset[rel_extreme == TRUE, .N, .(lon, lat, rel_event_id)]
dataset[rel_extreme == TRUE, rel_consec_days := .N, .(rel_event_id, lon, lat)]
dataset_rel_extremes <- unique(dataset[rel_extreme == TRUE, .(lon, lat, month = month(date), year = year(date), rel_event_id, consec_days)])
dataset_rel_extremes_yr <- dataset_rel_extremes[, .(rel_n_events = .N, 
                                            mean_rel_consec_days = mean(consec_days),
                                            max_rel_consec_days = max(consec_days)),
                                        .(year, lon, lat)]

to_plot <- dataset_rel_extremes[, .N, .(lon, lat, month)]
ggplot(to_plot) +
  geom_tile(aes(lon, lat, fill = N)) +
  scale_fill_gradient2(low = "navyblue", mid = "grey90", high = "darkred", midpoint = 50) +
  facet_wrap(~month) +
  theme_minimal()






n_events_slopes <- trend(dataset_extremes_yr[, .(lon, lat, date = year, value = n_events)])
mean_consec_days_slopes <- trend(dataset_extremes_yr[, .(lon, lat, date = year, value = mean_consec_days)])
max_consec_days_slopes <- trend(dataset_extremes_yr[, .(lon, lat, date = year, value = max_consec_days)])

ggplot(n_events_slopes) +
  geom_tile(aes(lon, lat, fill = slope)) +
  scale_fill_gradient2(low = "navyblue", mid = "grey90", high = "darkred", midpoint = 0) +
  theme_minimal()

ggplot(mean_consec_days_slopes) +
  geom_tile(aes(lon, lat, fill = slope)) +
  scale_fill_gradient2(low = "navyblue", mid = "grey90", high = "darkred", midpoint = 0) +
  theme_minimal()

ggplot(max_consec_days_slopes) +
  geom_tile(aes(lon, lat, fill = slope)) +
  scale_fill_gradient2(low = "navyblue", mid = "grey90", high = "darkred", midpoint = 0) +
  theme_minimal()

  
#################################

test <- dataset[lon == 33.875 & lat == 4.875]

ggplot(test) +
  geom_line(aes(date, value)) +
  theme_minimal()

test_ordered <- test[order(value)]

test_ordered[, value_cum := cumsum(value)]
ggplot(test_ordered) +
  geom_line(aes(1:nrow(test), value_cum)) +
  theme_minimal()

test_annual <- test[, .(q90_to_mean =  quantile(value, 0.9) / mean(value)), year(date)]

ggplot(test_annual) + 
  geom_line(aes(year, q90_to_mean), col = 'black') +
  geom_smooth(aes(year, q90_to_mean), col = 'black', method = 'lm') +
  theme_minimal()

test[extreme == TRUE, consec_days := .N, event_id]
test_extremes <- unique(test[extreme == TRUE, .(lon, lat, year = year(date), event_id, consec_days)])
test_extremes[, .N, year]
to_plot <- test_extremes[, value := mean(consec_days), year]
ggplot(to_plot) +
  geom_line(aes(year, value), col = 'black') +
  geom_smooth(aes(year, value), col = 'black', method = 'lm') +
  theme_minimal()

test_cumsum <- test[, .(value = cumsum(value)), year(date)]

ggplot(test_cumsum) +
  geom_line
