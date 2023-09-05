source('source/main.R')
source('source/graphics.R')
source('source/era5land_budget.R')

basins_eval <- readRDS(paste0(PATH_SAVE_ERA5LAND_BUDGET, 'basins_eval.rds'))

start_year <- 1960
end_year <- 2019

basins_eval <- basins_eval[year(date) >= start_year & year(date) <= end_year]

basins_eval_tab <- dcast(basins_eval, formula = date + basin  ~ variable + dataset, value.var = 'value')  
basins_eval_tab[, pe_era5 := prec_era5 - `evap_era5-land`]
basins_eval_tab[, pe_cru := `prec_cru-ts` - `evap_gleam`]
basins_eval_tab[, `pe_em-earth` := `prec_em-earth` - `evap_gleam`]
basins_eval_tab[, pe_gpcc := prec_gpcc - `evap_gleam`]
basins_eval_tab[, pe_terraclimate := prec_terraclimate - evap_terraclimate]
basins_eval_tab[, `pe_gldas-noah` := `prec_gldas-noah` - `evap_gldas-noah`]

basin_storage_era5 <- basins_eval_tab[, .(date, storage = cumsum(pe_era5 - `runoff_era5-land`)), .(basin)]
basin_storage_terraclimate <- basins_eval_tab[, .(date, storage = cumsum(pe_terraclimate - runoff_terraclimate)), .(basin)]
basin_storage_gldas <- basins_eval_tab[, .(date, storage = cumsum(`pe_gldas-noah` - `runoff_gldas-noah`)), .(basin)]

gg_era5_single <- ggplot(basin_storage_era5) +
  geom_hline(yintercept = 0, col = 'grey50')+ 
  geom_line(aes(x = date, y = storage, col = basin)) + 
  xlab('Date (year)') +
  scale_x_date(limits = as.Date(c("1960-01-01", "2020-01-01"))) +
  ylab(expression(paste('Cumulative ', italic('P - E - R  '), '(mm)'))) +
  theme_light() + 
  scale_color_manual(name = "Basin", labels = c("Danube", "Mahanadi", "Gulf of Mexico", "Shebelli n Juba"), 
                     values = colset_mid_qual[c(1, 3, 5, 12)]) +
  theme(panel.background = element_rect(colour = "black"))

basins_full_name<- c("Danube", "Mahanadi", "Gulf of Mexico", "Shebelli n Juba")
names(basins_full_name) <- c('danube', 'mahanadi', 'mexico', 'shebelli')

gg_era5 <- ggplot(basin_storage_era5) +
  geom_hline(yintercept = 0, col = 'grey50')+ 
  geom_line(aes(x = date, y = storage, col = basin)) + 
  xlab('Date (year)') +
  scale_x_date(limits = as.Date(c("1960-01-01", "2020-01-01"))) +
  ylab(expression(paste('Cumulative ', italic('P - E - R  '), '(mm)'))) +
  theme_light() + 
  scale_color_manual(values = colset_mid_qual[c(1, 3, 5, 12)]) +
  theme(panel.background = element_rect(colour = "black")) +
  facet_wrap(~basin, scale = 'free', labeller = as_labeller(basins_full_name))+ 
  theme(strip.background = element_rect(fill = 'grey30', size=1.5, linetype="solid")) +
  theme(legend.position = "none")

gg_terraclimate <- ggplot(basin_storage_terraclimate) +
  geom_hline(yintercept = 0, col = 'grey50')+ 
  geom_line(aes(x = date, y = storage, col = basin)) + 
  xlab('Date (year)') +
  scale_x_date(limits = as.Date(c("1960-01-01", "2020-01-01"))) +
  ylab(expression(paste('Cumulative ', italic('P - E - R  '), '(mm)'))) +
  theme_light() + 
  scale_color_manual(values = colset_mid_qual[c(1, 3, 5, 12)]) +
  theme(panel.background = element_rect(colour = "black")) +
  facet_wrap(~basin, scale = 'free', labeller = as_labeller(basins_full_name))+ 
  theme(strip.background = element_rect(fill = 'grey30', size=1.5, linetype="solid")) +
  theme(legend.position = "none")

gg_gldas <- ggplot(basin_storage_gldas) +
  geom_hline(yintercept = 0, col = 'grey50')+ 
  geom_line(aes(x = date, y = storage, col = basin)) + 
  xlab('Date (year)') +
  scale_x_date(limits = as.Date(c("1960-01-01", "2020-01-01"))) +
  ylab(expression(paste('Cumulative ', italic('P - E - R  '), '(mm)'))) +
  theme_light() + 
  scale_color_manual(values = colset_mid_qual[c(1, 3, 5, 12)]) +
  theme(panel.background = element_rect(colour = "black")) +
  facet_wrap(~basin, scale = 'free', labeller = as_labeller(basins_full_name))+ 
  theme(strip.background = element_rect(fill = 'grey30', size=1.5, linetype="solid")) +
  theme(legend.position = "none")

ggarrange(gg_era5, gg_terraclimate, gg_gldas, 
                      labels = c('a', 'b', 'c'), align = 'hv',
                      nrow = 3, ncol = 1)
ggsave(paste0(PATH_SAVE_ERA5LAND_BUDGET_FIGURES, "cumsum_storage_1.png"), width = 6, height = 12)

ggarrange(gg_era5_single, gg_terraclimate, gg_gldas, 
                      labels = c('a', 'b', 'c'), align = 'hv',
                      nrow = 3, ncol = 1)
ggsave(paste0(PATH_SAVE_ERA5LAND_BUDGET_FIGURES, "cumsum_storage_2.png"), width = 6, height = 12)


basin_storage_era5 <- basins_eval_tab[, .(date, storage = cumsum(pe_era5 - `runoff_era5-land`)), .(basin)]

saveRDS(basins_eval_tab, paste0(PATH_SAVE_ERA5LAND_BUDGET, 'basins_eval_tab.rds'))

### Older plots 

ggplot(basins_eval_tab) +
  geom_point(aes(x = pe_terraclimate, y = runoff_terraclimate), col = 'red',) +
  geom_smooth(aes(x = pe_terraclimate, y = runoff_terraclimate), col = 'red', method = 'lm', se = 0) +
  geom_point(aes(x = `pe_gldas-noah`, y =  `runoff_gldas-noah`), col = 'blue') +
  geom_smooth(aes(x = `pe_gldas-noah`, y =  `runoff_gldas-noah`), col = 'blue', method = 'lm', se = 0) +
  geom_point(aes(x = pe_era5, y =  `runoff_era5-land`), col = 'green') +
  geom_smooth(aes(x = pe_era5, y =  `runoff_era5-land`), method = 'lm', col = 'green', se = 0) +
  geom_abline(slope = 1) +
  facet_wrap(~basin, scales = 'free') 

ggplot(basins_eval_tab) +
  geom_point(aes(x = pe_gpcc, y = pe_era5), alpha = 0.3) +
  geom_smooth(aes(x = pe_gpcc, y = pe_era5), col = 'red', method = 'lm') +
  geom_point(aes(x = pe_cru, y = pe_era5), alpha = 0.3) +
  geom_smooth(aes(x = pe_cru, y = pe_era5), col = 'blue', method = 'lm') +
  geom_abline(slope = 1) +
  facet_wrap(~basin, scale = 'free')

ggplot(basins_eval_tab) +
  geom_point(aes(x = prec_era5, y = prec_gpcc), alpha = 0.3) +
  geom_smooth(aes(x = prec_era5, y = prec_gpcc), col = 'red', method = 'lm') +
  geom_point(aes(x = prec_terraclimate, y = prec_gpcc), alpha = 0.3) +
  geom_smooth(aes(x = prec_terraclimate, y = prec_gpcc), col = 'blue', method = 'lm') +
  geom_abline(slope = 1) +
  facet_wrap(~basin, scale = 'free')

ggplot(basins_eval_tab) +
  geom_point(aes(x = `evap_era5-land`, y = evap_gleam), alpha = 0.3) +
  geom_smooth(aes(x = `evap_era5-land`, y = evap_gleam), col = 'red', method = 'lm') +
  geom_point(aes(x = evap_terraclimate, y = evap_gleam), alpha = 0.3) +
  geom_smooth(aes(x = evap_terraclimate, y = evap_gleam), col = 'blue', method = 'lm') +
  geom_abline(slope = 1) +
  facet_wrap(~basin, scale = 'free')

ggplot(basins_eval_tab) +
  geom_point(aes(x = pe_gpcc, y = pe_era5), alpha = 0.3) +
  geom_smooth(aes(x = pe_gpcc, y = pe_era5), col = 'red', method = 'lm') +
  geom_point(aes(x = pe_cru, y = pe_era5), alpha = 0.3) +
  geom_smooth(aes(x = pe_cru, y = pe_era5), col = 'blue', method = 'lm') +
  facet_wrap(~basin, scale = 'free')

ggplot(basins_eval_tab) +
  geom_point(aes(x = pe_gpcc, y = pe_era5), alpha = 0.3) +
  geom_smooth(aes(x = pe_gpcc, y = pe_era5), col = 'red', method = 'lm') +
  geom_point(aes(x = pe_cru, y = pe_era5), alpha = 0.3) +
  geom_smooth(aes(x = pe_cru, y = pe_era5), col = 'blue', method = 'lm') +
  geom_point(aes(x = `pe_em-earth`, y = pe_era5), alpha = 0.3) +
  geom_smooth(aes(x = `pe_em-earth`, y = pe_era5), col = 'green', method = 'lm') +
  geom_point(aes(x = pe_noah, y = pe_era5), alpha = 0.3) +
  geom_smooth(aes(x = pe_noah, y = pe_era5), col = 'black', method = 'lm') +
  geom_point(aes(x = pe_terra, y = pe_era5), alpha = 0.3) +
  geom_smooth(aes(x = pe_terra, y = pe_era5), col = 'black', method = 'lm') +
  facet_wrap(~basin, scale = 'free')

ggplot(basins_eval_tab) +
  geom_point(aes(x = pe_gpcc, y = pe_era5), alpha = 0.3) +
  geom_point(aes(x = pe_cru, y = pe_era5), alpha = 0.3) +
  geom_point(aes(x = `pe_em-earth`, y = pe_era5), alpha = 0.3) +
  geom_point(aes(x = pe_era5, y = pe_era5), alpha = 0.3) +
  geom_point(aes(x = pe_terra, y = pe_era5), alpha = 0.3) +
  facet_wrap(~basin)

to_plot <- melt(basins_eval_tab, id = 1:2)
to_plot[, c("variable", "dataset") := tstrsplit(variable, "_", fixed = TRUE)]

ggplot(to_plot[variable == 'pe']) +
  geom_boxplot(aes(y = value, fill = dataset)) +
  facet_wrap(~basin, scales = 'free') +
  theme_minimal()

ggplot(to_plot[variable == 'pe']) +
  geom_line(aes(x = date, y = value, col = dataset)) +
  facet_wrap(~basin) +
  theme_minimal()

to_plot <- dcast(to_plot,  date + basin + dataset ~ variable, value.var = 'value')
