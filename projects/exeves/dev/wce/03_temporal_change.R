source('source/exeves.R')
region <- 'wce'

exeves <- readRDS(paste0(PATH_OUTPUT_DATA, 'exeves_std_', region, '.rds'))
prec <- readRDS(paste0(PATH_OUTPUT_DATA, region, '_prec_grid.rds'))

names(exeves)[5] <- "evap"
names(prec)[3] <- "prec"

exeves_prec <- merge(exeves, prec, all.x = TRUE, by = c("grid_id", "date"))

evap_prec_exeves <- melt(exeves_prec[, .(grid_id, date, season, evap, prec, event_id)], id.vars = c('grid_id', 'date', 'season', 'event_id'))
evap_prec_exeves[, year := year(date)]
evap_prec_exeves[, month := month(date)]

evap_prec_exeves[, Conditions := ordered('ExEvE')]
evap_prec_exeves[is.na(event_id), Conditions :=  ordered('non-ExEvE')]

evap_prec_month_exeves <- evap_prec_exeves[, .(mean_month = sum(value)), .(variable, month, season, year, Conditions, grid_id)]
evap_prec_month_exeves <- evap_prec_month_exeves[, .(mean_month = sum(mean_month)), .(variable, month, season, year, Conditions, grid_id)]
evap_prec_month_exeves <- dcast(evap_prec_month_exeves, month + year + season + grid_id + Conditions ~ variable)
evap_prec_month_exeves[, p_e_diff := prec - evap]
evap_prec_month_exeves[, p_e_mean := (prec + evap)/2]
evap_prec_month_exeves <- melt(evap_prec_month_exeves, id.vars = c('month', 'year', 'season', 'grid_id', 'Conditions'))

evap_prec_region_mean_exeves <- evap_prec_month_exeves[, .(mean_month = mean(value)), .(variable, month, season, Conditions, year)]
evap_prec_region_mean_exeves[, mean_conditions := sum(mean_month), .(Conditions, year, variable)]
evap_prec_region_mean_exeves[, mean_season := sum(mean_month), .(year, season, Conditions, variable)]
evap_prec_region_mean_exeves[, mean_year := sum(mean_month), .(year, Conditions, variable)]
evap_prec_region_mean_exeves <- melt(evap_prec_region_mean_exeves, id.vars = c('variable', 'month', 'season', 'Conditions', 'year'))

names(evap_prec_region_mean_exeves)[6] <- 'time_scale'

to_plot <- unique(evap_prec_region_mean_exeves[time_scale == 'mean_year', .(variable, year, Conditions, value)])
to_plot <- dcast(to_plot, variable + year ~ Conditions )
to_plot$All <- to_plot$ExEvE + to_plot$`non-ExEvE`
to_plot <- to_plot[, .(year, variable, All, ExEvE, `non-ExEvE`)]
to_plot <- melt(to_plot, id.vars = c("year", 'variable'))
names(to_plot)[3] <- 'Conditions'

ggplot(to_plot) + 
  geom_line(aes(x = year, y = value, col = variable)) +
  facet_wrap(~factor(Conditions), scales = 'free') +
  theme_linedraw()

to_plot <- unique(evap_prec_region_mean_exeves[time_scale == 'mean_season', .(variable, season, year, Conditions, value)])
to_plot <- dcast(to_plot, variable + season + year ~ Conditions )
to_plot$All <- to_plot$ExEvE + to_plot$`non-ExEvE`
to_plot <- to_plot[, .(season, year, variable, All, ExEvE, `non-ExEvE`)]
to_plot <- melt(to_plot, id.vars = c("year", "season", 'variable'))
names(to_plot)[4] <- 'Conditions'

ggplot(to_plot, aes(x = year, y = value, col = variable)) + 
  geom_line() +
  geom_smooth(method = 'glm', se = 0) +
  facet_wrap(Conditions ~ season, scales = 'free') +
  theme_linedraw()

ggsave(paste0(PATH_OUTPUT_FIGURES, "temporal_change.png"), width = 12, height = 8)

install.packages("corrplot")
library(corrplot)
for_cor <- dcast(to_plot, year ~ variable + Conditions)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor(for_cor[, -1]),  method="color", col=col(200),
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, 
         diag = FALSE 
)

#Conditions?
for_cor <- dcast(to_plot[Conditions == 'All'], year ~ variable)
corrplot(cor(for_cor[, -1]), method = "pie")
for_cor <- dcast(to_plot[Conditions == 'ExEvE'], year ~ variable)
corrplot(cor(for_cor[, -1]), method = "pie")
for_cor <- dcast(to_plot[Conditions == 'non-ExEvE'], year ~ variable)
corrplot(cor(for_cor[, -1]), method = "pie")

#Two periods?
for_cor <- dcast(to_plot[year <= 2000], year ~ variable + Conditions)
corrplot(cor(for_cor[, -1]), method = "pie")

for_cor <- dcast(to_plot[year > 2000], year ~ variable + Conditions)
corrplot(cor(for_cor[, -1]), method = "pie")


#Seasons

