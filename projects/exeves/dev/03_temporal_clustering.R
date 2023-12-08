source('source/exeves.R')
library(stats)
library(HKprocess)

region <- 'czechia'
exeves <- readRDS(paste0(PATH_OUTPUT_DATA, 'exeves_std_', region, '.rds'))
max_lag <- 10
n_grids <- exeves[, max(grid_id)]

dummy <- exeves[, .(grid_id, std_value)]
exeves_acf <- dummy[, sapply(.SD, function(x) acf(x, lag.max = max_lag, plot = FALSE)$acf), grid_id]
exeves_acf[, lag := rep(1:(1 + max_lag), n_grids)]
acf_table <- dcast(exeves_acf, grid_id~lag, value.var = 'V1')
acf_lag_means <- apply(acf_table, 2, mean)[-1]

apply(acf_table, 2, quantile, 0.95)
apply(acf_table, 2, quantile, 0.05)

evap_acf <- data.table(lag = 0:max_lag, acf_lag_means, 
           q05 = apply(acf_table, 2, quantile, 0.05)[-1], 
           q95 = apply(acf_table, 2, quantile, 0.95)[-1])

ar_model_ensemble <- replicate(1000, arima.sim(model = list(order = c(1, 0, 0), ar = acf_lag_means[2]), n = 15340))
ensemble_acf <- apply(ar_model_ensemble, 2, acf, max_lag, plot = FALSE)
ensemble_acf_lag_means <- sapply(ensemble_acf, '[[', 1)
evap_acf[, ar_mean := rowMeans(ensemble_acf_lag_means)]
evap_acf[, ar_q95 := apply(ensemble_acf_lag_means, 1, function(x) quantile(x, 0.95))]
evap_acf[, ar_q05 := apply(ensemble_acf_lag_means, 1, function(x) quantile(x, 0.05))]

ggplot(evap_acf, aes(x = lag)) +
  geom_hline(yintercept = 0, col = 'grey50') + 
  geom_point(aes(y = acf_lag_means), col = colset_subdued_prof[2]) + 
  geom_line(aes(y = acf_lag_means), col = colset_subdued_prof[2], linewidth = 0.5) +
  geom_line(aes(y = q05), col = colset_subdued_prof[2], linetype = 3, linewidth = 0.5) +
  geom_line(aes(y = q95), col = colset_subdued_prof[2], linetype = 3, linewidth = 0.5) +
  geom_line(aes(y = ar_mean), col = colset_subdued_prof[1], linewidth = 0.5) +
  geom_point(aes(y = ar_mean), col = colset_subdued_prof[1]) + 
  geom_line(aes(y = ar_q05), col = colset_subdued_prof[1], linetype = 3, linewidth = 0.5) +
  geom_line(aes(y = ar_q95), col = colset_subdued_prof[1], linetype = 3, linewidth = 0.5) +
  xlab('Lag (day)') +
  ylab('Auto-correlation coef.') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, max_lag), breaks = seq(0, max_lag, 1)) +
  theme_linedraw()
ggsave(paste0(PATH_OUTPUT_FIGURES, "acf.png"), width = 5, height = 6)

mleHK(exeves[grid_id == 250, .(std_value)]$std_value)
