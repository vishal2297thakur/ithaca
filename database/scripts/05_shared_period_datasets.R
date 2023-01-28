### Creates subsets of datasets that share common time period for each variable


##Precipitation

#06/2000-12/2019
prec_fnames_2000_2019 <- c(list.files(path = path_prec_sim, full.names = TRUE),
                           list.files(path = path_prec_obs, full.names = TRUE))
prec_fnames_2000_2019 <- grep("land", prec_fnames_2000_2019, value = TRUE)[c(3, 7:8, 10:15, 17:22)]

prec_fnames_short_2000_2019 <- c(list.files(path = path_prec_sim),
                                 list.files(path = path_prec_obs))
prec_fnames_short_2000_2019  <- grep("land", prec_fnames_short_2000_2019, value = TRUE)[c(3, 7:8, 10:15, 17:22)]
prec_fnames_short_2000_2019 <- strsplit(prec_fnames_short_2000_2019, split = '_', fixed = TRUE)
prec_fnames_short_2000_2019 <- sort(sapply(prec_fnames_short_2000_2019, "[[", 1))

#01/1980-12/2019
prec_fnames_1980_2019 <- prec_fnames_2000_2019[c(2, 4:8, 10:14)]
prec_fnames_short_1980_2019 <- prec_fnames_short_2000_2019[c(2, 4:9, 11:14)] # DANGER: due to sort

#01/1960-12/2019
prec_fnames_1960_2019 <- prec_fnames_1980_2019[c(3:5, 8:9, 11)]
prec_fnames_short_1960_2019 <- prec_fnames_short_1980_2019[c(3:6, 10:11)]

save(prec_fnames_2000_2019, prec_fnames_short_2000_2019, 
     prec_fnames_1980_2019, prec_fnames_short_1980_2019,
     prec_fnames_1960_2019, prec_fnames_short_1960_2019, file = '~/shared/data_projects/ithaca/misc/common_periods.Rdata')
