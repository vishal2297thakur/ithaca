# Demonstrates agreement types

source('source/partition_prec.R')
source('source/geo_functions.R')

prec_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_datasets.rds"))

test <- prec_datasets[dataset %in% c('gpcc', 'gpcp')]
dummy <- dcast(test, lon + lat + year ~ dataset, value.var = 'prec')
dummy <- dummy[, .(rel_bias = mean(2 * (gpcc - gpcp) / (gpcc + gpcp)), cor_coef = cor(gpcc, gpcp)), .(lon, lat)]
dummy[rel_bias < 0.1 & rel_bias > -0.1 & cor_coef > 0.8, agreement_type := factor('both_agree')]
dummy[(rel_bias > 0.1 | rel_bias < -0.1) & cor_coef > 0.8, agreement_type := factor('correlation')]
dummy[(rel_bias < 0.1 & rel_bias > -0.1 ) & cor_coef < 0.8, agreement_type := factor('mean')]
dummy[(rel_bias > 0.1 | rel_bias < -0.1) & cor_coef < 0.8, agreement_type := factor('both_disagree')]
dummy[, .N, agreement_type]

ggplot(test[lon == -179.875 & lat == 66.625]) +
  geom_line(aes(x = as.numeric(year), y = prec, col = dataset)) +
  theme_linedraw()

ggplot(test[lon == -114.125 & lat == 77.875]) +
  geom_line(aes(x = as.numeric(year), y = prec, col = dataset)) +
  theme_linedraw()

ggplot(test[lon == 179.625 & lat == 51.875]) +
  geom_line(aes(x = as.numeric(year), y = prec, col = dataset)) +
  theme_linedraw()

ggplot(test[lon == -179.875 & lat == 65.125]) +
  geom_line(aes(x = as.numeric(year), y = prec, col = dataset)) +
  theme_linedraw()

ggplot(test[lon == -179.875 & lat == 65.125]) +
  geom_line(aes(x = as.numeric(year), y = prec, col = dataset)) +
  theme_linedraw()

ggplot(test[lon == 179.875 & lat == 68.875]) +
  geom_line(aes(x = as.numeric(year), y = prec, col = dataset)) +
  theme_linedraw()

ggplot(test[lon == -164.375 & lat ==  62.625]) +
  geom_line(aes(x = as.numeric(year), y = prec, col = dataset)) +
  theme_linedraw()

test <- prec_datasets[dataset %in% c('precl', 'jra55')]
dummy <- dcast(test, lon + lat + year ~ dataset, value.var = 'prec')
dummy <- dummy[, .(rel_bias = mean(2 * (precl - jra55) / (precl + jra55)), cor_coef = cor(precl, jra55)), .(lon, lat)]
dummy[rel_bias < 0.1 & rel_bias > -0.1 & cor_coef > 0.8, agreement_type := factor('both_agree')]
dummy[(rel_bias > 0.1 | rel_bias < -0.1) & cor_coef > 0.8, agreement_type := factor('correlation')]
dummy[(rel_bias < 0.1 & rel_bias > -0.1 ) & cor_coef < 0.8, agreement_type := factor('mean')]
dummy[(rel_bias > 0.1 | rel_bias < -0.1) & cor_coef < 0.8, agreement_type := factor('both_disagree')]
dummy[, .N, agreement_type]

ggplot(test[lon == -179.875 & lat == 66.625]) +
  geom_line(aes(x = as.numeric(year), y = prec, col = dataset)) +
  theme_linedraw()

ggplot(test[lon == -114.125 & lat == 77.875]) +
  geom_line(aes(x = as.numeric(year), y = prec, col = dataset)) +
  theme_linedraw()

ggplot(test[lon == 179.625 & lat == 51.875]) +
  geom_line(aes(x = as.numeric(year), y = prec, col = dataset)) +
  theme_linedraw()

ggplot(test[lon == -179.875 & lat == 65.125]) +
  geom_line(aes(x = as.numeric(year), y = prec, col = dataset)) +
  theme_linedraw()

ggplot(test[lon == -179.875 & lat == 65.125]) +
  geom_line(aes(x = as.numeric(year), y = prec, col = dataset)) +
  theme_linedraw()

ggplot(test[lon == 179.875 & lat == 68.875]) +
  geom_line(aes(x = as.numeric(year), y = prec, col = dataset)) +
  theme_linedraw()

ggplot(test[lon == -164.375 & lat ==  62.625]) +
  geom_line(aes(x = as.numeric(year), y = prec, col = dataset)) +
  theme_linedraw()

-167.875  53.375
