source('source/exeves.R')

region <- 'wce'
exeves <- readRDS(paste0(PATH_OUTPUT_DATA, 'exeves_std_', region, '.rds'))
prec <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_prec_grid.rds'))

names(prec)[3] <- "prec"
exeves_prec <- merge(exeves[!is.na(event_id)], prec, by = c('grid_id', 'date'), all.x = TRUE)
names(exeves_prec)[5] <- 'evap'

exeves_prec[!is.na(extreme_id), extreme_evap := TRUE]

#Day of precipitation
exeves_prec[, event_day := seq_len(.N), by = .(event_id, grid_id)]
exeves_prec[, extreme_day := seq_len(.N), by = .(extreme_id, grid_id)]
exeves_prec[, event_duration := .N, by = .(event_id, grid_id)]
exeves_prec[, extremes_per_event := .N, by = .(extreme_id, event_id, grid_id)]
exeves_prec[is.na(extreme_id), extreme_day := NA]
exeves_prec[is.na(extreme_id), extremes_per_event := NA]
exeves_prec[extreme_day == 1, event_day_of_extreme := event_day]
exeves_prec[, cumsum_evap := cumsum(evap), .(grid_id, event_id)]
exeves_prec[, cumsum_prec := cumsum(prec), .(grid_id, event_id)]
exeves_prec[, cumsum_diff := cumsum_prec - cumsum_evap, .(grid_id, event_id)]

prec_max <- exeves_prec[exeves_prec[, prec == max(prec), .(event_id, grid_id)]$V1]

prec_max <- prec_max[, .(date, grid_id, event_id, prec_max = prec)]
exeves_prec <- merge(exeves_prec, prec_max, by = c('grid_id', 'event_id', 'date'), all.x = TRUE)
exeves_prec[!is.na(prec_max), event_day_of_prec_max := event_day]

exeves_prec[, event_day_of_first_extreme := min(event_day_of_extreme, na.rm = T) , .(grid_id, event_id)]
exeves_prec[, event_day_of_first_prec_max:= min(event_day_of_prec_max, na.rm = T) , .(grid_id, event_id)]

to_plot <- unique(exeves_prec[, .(grid_id, event_id, event_duration,
                                  event_day_of_first_extreme, 
                                  event_day_of_first_prec_max, 
                                  month = month(date), 
                                  period)])
#to_plot <- to_plot[!is.infinite(rowSums(to_plot)), ]
to_plot[, mean_lag := mean(event_day_of_first_extreme - event_day_of_first_prec_max, na.rm = T), 
        .(grid_id, event_id, period)]

ggplot(to_plot[event_duration %in% c(6, 8, 10)]) + #USE THIS!
  geom_vline(xintercept = 1, col = 'black') +
  geom_hline(yintercept = 0, col = 'black') +
  geom_density(aes(x = event_day_of_first_prec_max, group = event_duration), stat = "count", col = 'blue') +
  geom_density(aes(x = event_day_of_first_extreme, group = event_duration), stat = "count", col = 'red' ) +
  theme_linedraw()

ggplot(to_plot[event_duration %in% c(6, 8, 10)]) + #USE THIS!
  geom_vline(xintercept = 0, col = 'black') +
  geom_density(aes(x = -mean_lag, col = factor(event_duration)), stat = "count") +
  theme_linedraw()


ggplot(to_plot[event_duration %in% c(6, 8, 10) & month %in% c(1, 6)]) +
  geom_vline(xintercept = 1, col = 'black') +
  geom_hline(yintercept = 0, col = 'black') +
  geom_density(aes(x = event_day_of_first_prec_max, group = event_duration), stat = "count", col = 'blue') +
  geom_density(aes(x = event_day_of_first_extreme, group = event_duration), stat = "count", col = 'red' ) +
  facet_wrap(~factor(month)) +
  theme_linedraw()

ggplot(to_plot[event_duration %in% c(3, 5, 7, 9) & month %in% c(1, 6)]) + 
  geom_vline(xintercept = 0, col = 'black') +
  geom_density(aes(x = -mean_lag, col = factor(month)), stat = "count") +
  facet_wrap(~factor(event_duration), scales = 'free_y') +
  theme_minimal()

to_plot <- unique(exeves_prec[, .(grid_id, event_id, event_day, period,
                                  event_duration, 
                                  cumsum_diff,
                                  cumsum_prec,
                                  month = month(date))])
to_plot <- to_plot[, .(cumsum_diff = mean(cumsum_diff), cumsum_prec = mean(cumsum_prec)), .(event_day, event_duration, period)]

ggplot(to_plot[event_duration == 9]) +   #USE THIS!!!
  geom_point(aes(y = cumsum_diff, x = factor(event_day), col = factor(period))) +
  geom_line(aes(y = cumsum_diff, x = factor(event_day), group = interaction(period, event_duration), col = period)) +
  geom_hline(yintercept = 0, col = 'grey20') +
  theme_minimal()

ggplot(to_plot[event_duration <= 10 ]) +   #USE THIS!!!
  geom_point(aes(y = cumsum_diff, x = factor(event_day), col = factor(period))) +
  geom_line(aes(y = cumsum_diff, x = factor(event_day), group = interaction(period, event_duration), col = period)) +
  geom_hline(yintercept = 0, col = 'grey20') +
  facet_wrap(~event_duration)
  theme_minimal()

grid_cell_n <- length(unique(exeves_prec$grid_id))

to_plot <- unique(exeves_prec[, .(grid_id, event_id, event_day, period,
                                  cumsum_diff,
                                  cumsum_prec)])
to_plot <- to_plot[, .(cumsum_diff = mean(cumsum_diff), cumsum_prec = mean(cumsum_prec)), .(event_day, period)]

ggplot(to_plot[event_day < 11 & event_day > 1]) +  #CHECK THIS !!!
  geom_point(aes(y = cumsum_diff, x = factor(event_day), col = factor(period))) +
  geom_line(aes(y = cumsum_diff, x = factor(event_day), group = period, col = period)) +
  geom_hline(yintercept = 0, col = 'grey20') +
  theme_minimal()

ggplot(to_plot[event_day < 11 & event_day > 1]) +  #CHECK THIS !!!
  geom_point(aes(y = cumsum_prec, x = factor(event_day), col = factor(period))) +
  geom_line(aes(y = cumsum_prec, x = factor(event_day), group = period, col = period)) +
  geom_hline(yintercept = 0, col = 'grey20') +
  theme_minimal()


