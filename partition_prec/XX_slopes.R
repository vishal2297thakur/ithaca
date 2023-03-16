source('source/partition_prec.R')
source('source/geo_functions.R')
source('source/graphics.R')

prec_2000_2019 <- lapply(PREC_FNAMES_2000_2019, brick)
names(prec_2000_2019) <- PREC_FNAMES_SHORT_2000_2019

registerDoParallel(cores = N_CORES - 1)
prec_slopes <- foreach(dataset_count = 1:n_datasets_2000_2019) %dopar% {
  dummie <- prec_2000_2019[[dataset_count]]
  dummie_slopes <- brick_slopes(dummie, annual = 'sum')
  dummie_table <- data.table(as.data.frame(dummie_slopes, xy = TRUE, na.rm = TRUE))
  dummie_table <- dummie_table[layer != 0]
  dummie_table[, .(lon = x, lat = y, slope = layer,
                   dataset = names(prec_2000_2019)[dataset_count])]
}
prec_slopes <- rbindlist(prec_slopes)
prec_slopes <- prec_slopes[lat >= -70]
prec_slopes[slope < 0, slope_sign := factor('neg')]
prec_slopes[slope > 0, slope_sign := factor('pos')]

prec_slopes[slope_sign == 'neg', .N, dataset]
prec_slopes[slope_sign == 'pos', .N, dataset]
prec_slopes[, .N, .(dataset)]

slope_median <- prec_slopes[, .(slope = median(slope)), .(lon, lat)]
slope_agreement <- prec_slopes[slope > 2 | slope < -2, .N, .(slope_sign, lon, lat)] 

prec_slopes_neg <- reshape2::acast(prec_slopes[slope_sign == 'neg'], dataset~lon+lat, fun.aggregate = length)
prec_slopes_pos <- reshape2::acast(prec_slopes[slope_sign == 'pos'], dataset~lon+lat, fun.aggregate = length)

pos_slope_agreement <- replace(m <- crossprod(as.matrix(t(prec_slopes_pos))), lower.tri(m, diag = TRUE), NA)
neg_slope_agreement <- replace(m <- crossprod(as.matrix(t(prec_slopes_neg))), lower.tri(m, diag = TRUE), NA)


ggplot(prec_slopes[dataset == 'gpm-imerg' ]) +
  geom_point(aes(lon, lat, col = slope_sign)) +
  theme_bw()

ggplot(prec_slopes[slope > 5 | slope < -5]) +
  geom_point(aes(lon, lat, col = slope)) +
  theme_bw()

to_plot <- prec_slopes[slope > -50 & slope < 50]
to_plot[slope_sign == 'neg', neg_slope_rev := -slope]s
to_plot[slope_sign == 'pos', pos_slope := slope]

ggplot(to_plot) +
  geom_density(aes(pos_slope))+
  geom_density(aes(neg_slope_rev), col = 'red')+
  theme_bw()

ggplot(slope_agreement[slope_sign == 'neg' & N > 10]) +
  geom_point(aes(lon, lat, col = N)) +
  theme_bw()

ggplot(slope_agreement[slope_sign == 'pos' & N > 10]) +
  geom_point(aes(lon, lat, col = N)) +
  theme_bw()

saveRDS(prec_slopes, paste0(PATH_SAVE_PARTITION_PREC, "test_slopes.rds"))




