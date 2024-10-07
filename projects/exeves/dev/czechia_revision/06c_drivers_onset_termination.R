source('source/exeves.R')

region <- 'czechia'

exeves_drivers <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_exeves_drivers.rds'))

#Onset/termination dates
onset_date <- exeves_drivers[event_day == 1, .(date = date), grid_id]
termination_date <- exeves_drivers[event_day == event_duration, .(date = date + 1), grid_id]

# 
dummy <- exeves_drivers[is.na(event_80_95_id), mean(prec), .(month(date))]
prec_on_onset <- merge(onset_date, 
                       exeves_drivers[, .(date, grid_id, prec)], 
                       by = c('grid_id', 'date'), all.x = TRUE)
to_plot <- prec_on_onset[, .(variable = factor('Precipitation'), day = factor('Onset'), value = mean(prec, na.rm = T)) , .(month(date))]
to_plot <- to_plot[order(month)]
to_plot$value <- to_plot$value / dummy[order(month)]$V1

prec_on_termination <- merge(termination_date, 
                             exeves_drivers[, .(date, grid_id, prec)], 
                             by = c('grid_id', 'date'), all.x = TRUE)
to_plot_2 <- prec_on_termination[, .(variable = factor('Precipitation'), day = factor('Termination'), value = mean(prec, na.rm = T)) , .(month(date))]
to_plot_2 <- to_plot_2[order(month)]
to_plot_2$value <- to_plot_2$value / dummy[order(month)]$V1

to_plot <- rbind(to_plot, to_plot_2)

dummy <- exeves_drivers[is.na(event_80_95_id), mean(swrad), .(month(date))]
swrad_on_onset <- merge(onset_date, 
                        exeves_drivers[, .(date, grid_id, swrad)], 
                        by = c('grid_id', 'date'), all.x = TRUE)
to_plot_2 <- swrad_on_onset[, .(variable = factor('Shortwave'), day = factor('Onset'), value = mean(swrad, na.rm = T)) , .(month(date))]
to_plot_2 <- to_plot_2[order(month)]
to_plot_2$value <- to_plot_2$value / dummy[order(month)]$V1

to_plot <- rbind(to_plot, to_plot_2)

swrad_on_termination <- merge(termination_date, 
                              exeves_drivers[, .(date, grid_id, swrad)], 
                              by = c('grid_id', 'date'), all.x = TRUE)
to_plot_2 <- swrad_on_termination[, .(variable = factor('Shortwave'), day = factor('Termination'), value = mean(swrad, na.rm = T)) , .(month(date))]
to_plot_2 <- to_plot_2[order(month)]
to_plot_2$value <- to_plot_2$value / dummy[order(month)]$V1

to_plot <- rbind(to_plot, to_plot_2)


dummy <- exeves_drivers[is.na(event_80_95_id), mean(lwrad), .(month(date))]
lwrad_on_onset <- merge(onset_date, 
                        exeves_drivers[, .(date, grid_id, lwrad)], 
                        by = c('grid_id', 'date'), all.x = TRUE)
to_plot_2 <- lwrad_on_onset[, .(variable = factor('Longwave'), day = factor('Onset'), value = mean(lwrad, na.rm = T)) , .(month(date))]
to_plot_2 <- to_plot_2[order(month)]
to_plot_2$value <- to_plot_2$value / dummy[order(month)]$V1

to_plot <- rbind(to_plot, to_plot_2)

lwrad_on_termination <- merge(termination_date, 
                              exeves_drivers[, .(date, grid_id, lwrad)], 
                              by = c('grid_id', 'date'), all.x = TRUE)
to_plot_2 <- lwrad_on_termination[, .(variable = factor('Longwave'), day = factor('Termination'), value = mean(lwrad, na.rm = T)) , .(month(date))]
to_plot_2 <- to_plot_2[order(month)]
to_plot_2$value <- to_plot_2$value / dummy[order(month)]$V1

to_plot <- rbind(to_plot, to_plot_2)
colnames(to_plot)[3] <- "Day"

gg_onset <- ggplot(to_plot) +
  geom_line(aes(y = value, x = factor(month), col = Day, group = Day))+
  geom_point(aes(y = value, x = factor(month), col = Day, group = Day))+
  geom_hline(yintercept = 1) +
  facet_wrap(~variable, scales = 'free') +
  xlab("Month") +
  ylab("Ratio") +
  scale_color_manual(values = SUBDUED_PROF_PALETTE[c(2, 1)]) +
  theme_linedraw() + 
  theme(strip.background = element_rect(fill = 'grey30'))

### Before onset/After termination day

dates_before_onset <- exeves_drivers[event_day == 1, .(date = date - 1), grid_id]
dates_after_termination <- exeves_drivers[event_day == event_duration, .(date = date + 1), grid_id]

dummy <- exeves_drivers[is.na(event_80_95_id), mean(prec), .(month(date))]
prec_on_onset <- merge(dates_before_onset, 
                       exeves_drivers[, .(date, grid_id, prec)], 
                       by = c('grid_id', 'date'), all.x = TRUE)
to_plot <- prec_on_onset[, .(variable = factor('Precipitation'), day = factor('Before onset'), value = mean(prec, na.rm = T)) , .(month(date))]
to_plot <- to_plot[order(month)]
to_plot$value <- to_plot$value / dummy[order(month)]$V1

prec_on_termination <- merge(dates_after_termination, 
                             exeves_drivers[, .(date, grid_id, prec)], 
                             by = c('grid_id', 'date'), all.x = TRUE)
to_plot_2 <- prec_on_termination[, .(variable = factor('Precipitation'), day = factor('After termination'), value = mean(prec, na.rm = T)) , .(month(date))]
to_plot_2 <- to_plot_2[order(month)]
to_plot_2$value <- to_plot_2$value / dummy[order(month)]$V1

to_plot <- rbind(to_plot, to_plot_2)

dummy <- exeves_drivers[is.na(event_80_95_id), mean(swrad), .(month(date))]
swrad_on_onset <- merge(dates_before_onset, 
                        exeves_drivers[, .(date, grid_id, swrad)], 
                        by = c('grid_id', 'date'), all.x = TRUE)
to_plot_2 <- swrad_on_onset[, .(variable = factor('Shortwave'), day = factor('Before onset'), value = mean(swrad, na.rm = T)) , .(month(date))]
to_plot_2 <- to_plot_2[order(month)]
to_plot_2$value <- to_plot_2$value / dummy[order(month)]$V1

to_plot <- rbind(to_plot, to_plot_2)

swrad_on_termination <- merge(dates_after_termination, 
                              exeves_drivers[, .(date, grid_id, swrad)], 
                              by = c('grid_id', 'date'), all.x = TRUE)
to_plot_2 <- swrad_on_termination[, .(variable = factor('Shortwave'), day = factor('After termination'), value = mean(swrad, na.rm = T)) , .(month(date))]
to_plot_2 <- to_plot_2[order(month)]
to_plot_2$value <- to_plot_2$value / dummy[order(month)]$V1

to_plot <- rbind(to_plot, to_plot_2)


dummy <- exeves_drivers[is.na(event_80_95_id), mean(lwrad), .(month(date))]
lwrad_on_onset <- merge(dates_before_onset, 
                        exeves_drivers[, .(date, grid_id, lwrad)], 
                        by = c('grid_id', 'date'), all.x = TRUE)
to_plot_2 <- lwrad_on_onset[, .(variable = factor('Longwave'), day = factor('Before onset'), value = mean(lwrad, na.rm = T)) , .(month(date))]
to_plot_2 <- to_plot_2[order(month)]
to_plot_2$value <- to_plot_2$value / dummy[order(month)]$V1

to_plot <- rbind(to_plot, to_plot_2)

lwrad_on_termination <- merge(dates_after_termination, 
                              exeves_drivers[, .(date, grid_id, lwrad)], 
                              by = c('grid_id', 'date'), all.x = TRUE)
to_plot_2 <- lwrad_on_termination[, .(variable = factor('Longwave'), day = factor('After termination'), value = mean(lwrad, na.rm = T)) , .(month(date))]
to_plot_2 <- to_plot_2[order(month)]
to_plot_2$value <- to_plot_2$value / dummy[order(month)]$V1

to_plot <- rbind(to_plot, to_plot_2)
colnames(to_plot)[3] <- "Day"
gg_before_onset <- ggplot(to_plot) +
  geom_line(aes(y = value, x = factor(month), col = Day, group = Day))+
  geom_point(aes(y = value, x = factor(month), col = Day, group = Day))+
  geom_hline(yintercept = 1) +
  facet_wrap(~variable, scales = 'free') +
  xlab("Month") +
  ylab("Ratio") +
  scale_color_manual(values = SUBDUED_PROF_PALETTE[c(2, 1)]) +
  theme_linedraw() + 
  theme(strip.background = element_rect(fill = 'grey30'))

ggarrange(gg_onset, NULL, gg_before_onset,
          nrow = 3, 
          labels = c("A", "", "B"), heights = c(1, 0.05, 1),
          legend = 'right', common.legend = TRUE) 
ggsave(paste0(PATH_OUTPUT_FIGURES, "onset_termination.png"), width = 8, height = 6)
