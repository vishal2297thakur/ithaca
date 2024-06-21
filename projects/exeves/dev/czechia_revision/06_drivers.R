source('source/exeves.R')
library(pRecipe)

region <- 'czechia'
exeves <- readRDS(paste0(PATH_OUTPUT_DATA, 'exeves_std_', region, '.rds'))
lwrad <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_lwrad_grid.rds'))
swrad <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_swrad_grid.rds'))
prec <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_prec_grid.rds'))

rad <- merge(lwrad[, .(grid_id, date, lwrad = value, std_lwrad = std_value)], 
             swrad[, .(grid_id, date, swrad = value, std_swrad = std_value)], 
             by = c('grid_id', 'date'))
exeves_drivers <- merge(exeves, rad, by = c('grid_id', 'date'))
exeves_drivers <- merge(exeves_drivers, 
                        prec[, .(grid_id, date, prec = value)], by = c('grid_id', 'date'))

exeves_drivers[, Conditions := ordered('ExEvE')]
exeves_drivers[is.na(event_80_95_id), Conditions :=  ordered('non-ExEvE')]
exeves_drivers[, total_rad := lwrad + swrad]

to_plot <- exeves_drivers[, .(lwrad = mean(std_lwrad), swrad = mean(std_swrad)), 
                          .(grid_id, month(date), Conditions)]
gg_rad <- ggplot(to_plot) +
  geom_point(aes(x = lwrad, y = swrad, col = Conditions), alpha = 0.5) + 
  geom_hline(yintercept = 0, col = colset_subdued_prof[3]) +
  geom_vline(xintercept = 0, col = colset_subdued_prof[3]) +
  facet_wrap(~month) +
  xlab("Longwave radiation (z-score)") +
  ylab("Shortwave radiation (z-score)") +
  scale_color_manual(values = colset_subdued_prof[c(4, 2)]) +
  theme_linedraw() +
  theme(axis.title = element_text(size = 12),
        axis.title.x = element_text(vjust = -0.5, 
                                    margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(angle = 90, vjust = -0.5, 
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)),
        strip.background = element_rect(fill = colset_subdued_prof[3]),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) 

to_plot <- exeves_drivers[, .(evap = mean(value), prec = mean(prec)), 
                          .(grid_id, month(date), Conditions)] 
gg_prec <- ggplot(to_plot) +
  geom_point(aes(x = evap, y = prec, col = Conditions), alpha = 0.7) +
  facet_wrap(~month, scales = "free") +
  xlab("Evaporation (mm/day)") +
  ylab("Precipitation (mm/day)") +
  scale_color_manual(values = colset_subdued_prof[c(4, 2)]) +
  theme_linedraw() +
  theme(axis.title = element_text(size = 12),
        axis.title.x = element_text(vjust = -0.5, 
                                    margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(angle = 90, vjust = -0.5, 
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)),
        strip.background = element_rect(fill = colset_subdued_prof[3]),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) 

ggarrange(gg_rad, NULL, gg_prec,
                     nrow = 3, 
                     labels = c("A", "", "B"), heights = c(1, 0.05, 1),
                     legend = 'bottom', common.legend = TRUE) + bgcolor("white")  
ggsave(paste0(PATH_OUTPUT_FIGURES, "drivers.png"), width = 7, height = 11)


# Wet days / Dry days

all_prec_days <- exeves_drivers[, .N/(161 * 0.5 * PERIOD_LENGTH), .(period, prec_day, month(date))]
colnames(all_prec_days)[4] <- "value"
all_prec_days$conditions <- "All days"

exeves_prec_days <- exeves_drivers[!is.na(event_80_95_id), .N/(161 * 0.5 * PERIOD_LENGTH), .(period, prec_day, month(date))]
colnames(exeves_prec_days)[4] <- "value"
exeves_prec_days$conditions <- "ExEvEs"

non_exeves_prec_days <- exeves_drivers[is.na(event_80_95_id), .N/(161 * 0.5 * PERIOD_LENGTH), .(period, prec_day, month(date))]
colnames(non_exeves_prec_days)[4] <- "value"
non_exeves_prec_days$conditions <- "Non-ExEvEs"

prec_days <- rbind(all_prec_days, exeves_prec_days, non_exeves_prec_days)
prec_days <- prec_days[, c(1, 3:2, 5:4)]

ggplot(prec_days[conditions == 'ExEvEs']) +
  geom_col(aes(x = period, y = value, fill = prec_day),
           position = position_fill()) +
  facet_wrap(~month)+
  theme_linedraw() 


exeves_drivers[, prec_day := factor("wet")]
exeves_drivers[prec < 1, prec_day := factor("dry")]


exeves_drivers[prec_day == 'dry', .N/(161 * 0.5 * PERIOD_LENGTH), .(period)]
exeves_drivers[!is.na(event_80_95_id), .N/(161 * 0.5 * PERIOD_LENGTH), .(period)]

to_plot <- exeves_drivers[prec_day == 'dry', .N/(SUB_PERIOD_YEARS), .(period, grid_id)]
colnames(to_plot)[c(1, 3)] <- c("Period", "Dry Days")
ggplot(to_plot) +
  geom_boxplot(aes(x = Period, y = `Dry Days`)) +
  theme_linedraw() 

to_plot <- exeves_drivers[prec_day == 'dry', .N/(SUB_PERIOD_YEARS), .(period, grid_id,  factor(month(date)))]
colnames(to_plot)[c(1, 3:4)] <- c("Period", "Month", "Dry Days")
ggplot(to_plot) +
  geom_boxplot(aes(x = Month, y = `Dry Days`, fill = Period)) +
  theme_linedraw() 


all_prec_days <- exeves_drivers[, .N/(161 * 0.5 * PERIOD_LENGTH), .(period, prec_day)]
colnames(all_prec_days)[3] <- "value"
all_prec_days$conditions <- "All days"

exeves_prec_days <- exeves_drivers[!is.na(event_80_95_id), .N/(161 * 0.5 * PERIOD_LENGTH), .(period, prec_day)]
colnames(exeves_prec_days)[3] <- "value"
exeves_prec_days$conditions <- "ExEvEs"

non_exeves_prec_days <- exeves_drivers[is.na(event_80_95_id), .N/(161 * 0.5 * PERIOD_LENGTH), .(period, prec_day)]
colnames(non_exeves_prec_days)[3] <- "value"
non_exeves_prec_days$conditions <- "Non-ExEvEs"

exeves_prec_days <- rbind(all_prec_days, exeves_prec_days, non_exeves_prec_days)
exeves_prec_days <- exeves_prec_days[, c(4, 1:3)]

ggplot(exeves_prec_days) +
  geom_col(aes(x = period, y = value, fill = prec_day, group = prec_day),
           position = position_dodge()) +
  facet_wrap(~conditions, scales = 'free')+
  theme_linedraw() 

all_prec_days <- exeves_drivers[month(date) == 6, .N/(161 * 0.5 * PERIOD_LENGTH), .(period, prec_day)]
colnames(all_prec_days)[3] <- "value"
all_prec_days$conditions <- "All days"

exeves_prec_days <- exeves_drivers[!is.na(event_80_95_id) & month(date) == 6, .N/(161 * 0.5 * PERIOD_LENGTH), .(period, prec_day)]
colnames(exeves_prec_days)[3] <- "value"
exeves_prec_days$conditions <- "ExEvEs"

non_exeves_prec_days <- exeves_drivers[is.na(event_80_95_id) & month(date) == 6, .N/(161 * 0.5 * PERIOD_LENGTH), .(period, prec_day)]
colnames(non_exeves_prec_days)[3] <- "value"
non_exeves_prec_days$conditions <- "Non-ExEvEs"

exeves_prec_days <- rbind(all_prec_days, exeves_prec_days, non_exeves_prec_days)
exeves_prec_days <- exeves_prec_days[, c(4, 1:3)]

ggplot(exeves_prec_days) +
  geom_col(aes(x = period, y = value, fill = prec_day, group = prec_day),
           position = position_dodge()) +
  facet_wrap(~conditions, scales = 'free')+
  theme_linedraw() 

all_prec_days <- exeves_drivers[month(date) == 1, .N/(161 * 0.5 * PERIOD_LENGTH), .(period, prec_day)]
colnames(all_prec_days)[3] <- "value"
all_prec_days$conditions <- "All days"

exeves_prec_days <- exeves_drivers[!is.na(event_80_95_id) & month(date) == 1, .N/(161 * 0.5 * PERIOD_LENGTH), .(period, prec_day)]
colnames(exeves_prec_days)[3] <- "value"
exeves_prec_days$conditions <- "ExEvEs"

non_exeves_prec_days <- exeves_drivers[is.na(event_80_95_id) & month(date) == 1, .N/(161 * 0.5 * PERIOD_LENGTH), .(period, prec_day)]
colnames(non_exeves_prec_days)[3] <- "value"
non_exeves_prec_days$conditions <- "Non-ExEvEs"

exeves_prec_days <- rbind(all_prec_days, exeves_prec_days, non_exeves_prec_days)
exeves_prec_days <- exeves_prec_days[, c(4, 1:3)]

ggplot(exeves_prec_days) +
  geom_col(aes(x = period, y = value, fill = prec_day, group = prec_day),
           position = position_dodge()) +
  facet_wrap(~conditions, scales = 'free')+
  theme_linedraw() 














exeves_drivers[Conditions == 'ExEvE', mean(total_rad)] /
  exeves_drivers[, mean(total_rad)]

exeves_drivers[Conditions == 'ExEvE', mean(swrad)] /
  exeves_drivers[Conditions == 'non-ExEvE', mean(swrad)]

exeves_drivers[Conditions == 'ExEvE', mean(lwrad)] /
  exeves_drivers[Conditions == 'non-ExEvE', mean(lwrad)]

exeves_drivers[Conditions == 'ExEvE', mean(swrad), month(date)] 
exeves_drivers[Conditions == 'non-ExEvE', mean(swrad), month(date)]

exeves_drivers[Conditions == 'ExEvE', mean(lwrad), month(date)] 
exeves_drivers[Conditions == 'non-ExEvE', mean(lwrad), month(date)]

exeves_drivers[Conditions == 'ExEvE',  mean(prec)] /
exeves_drivers[Conditions == 'non-ExEvE',  mean(prec)]

exeves_drivers[Conditions == 'ExEvE', mean(prec), month(date)] 
exeves_drivers[Conditions == 'non-ExEvE', mean(prec), month(date)]

exeves_drivers[Conditions == 'ExEvE', sum(prec), month(date)] 
exeves_drivers[Conditions == 'non-ExEvE', sum(prec), month(date)]

exeves_drivers[Conditions == 'ExEvE',  sum(prec)] /
  exeves_drivers[, sum(prec)]
