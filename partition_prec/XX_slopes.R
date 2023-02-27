prec_2000_2019 <- lapply(PREC_FNAMES_2000_2019, brick)
names(prec_2000_2019) <- PREC_FNAMES_SHORT_2000_2019 

dummy <- brick_slopes(prec_2000_2019[[1]], annual = 'sum')
dummy_dt <- data.table(as.data.frame(dummy, xy = TRUE))
dummy_dt[slope < 0, slope_sign := factor('neg')]
dummy_dt[slope > 0, slope_sign := factor('pos')]
ggplot(dummy_dt[slope > 5 | slope < -5]) +
  geom_point(aes(x, y, col = slope_sign))
