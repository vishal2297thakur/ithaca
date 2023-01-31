# Creates subsets that share common time period for each variable

##Precipitation

### 06/2000-12/2019
PREC_FNAMES_2000_2019 <- c(list.files(path = PATH_PREC_SIM, full.names = TRUE),
                           list.files(path = PATH_PREC_OBS, full.names = TRUE))
PREC_FNAMES_2000_2019 <- grep("land", PREC_FNAMES_2000_2019, value = TRUE)[c(3, 7:8, 10:15, 17:22)]

PREC_FNAMES_SHORT_2000_2019 <- c(list.files(path = PATH_PREC_SIM),
                                 list.files(path = PATH_PREC_OBS))
PREC_FNAMES_SHORT_2000_2019  <- grep("land", PREC_FNAMES_SHORT_2000_2019, value = TRUE)[c(3, 7:8, 10:15, 17:22)]
PREC_FNAMES_SHORT_2000_2019 <- strsplit(PREC_FNAMES_SHORT_2000_2019, split = '_', fixed = TRUE)
PREC_FNAMES_SHORT_2000_2019 <- sort(sapply(PREC_FNAMES_SHORT_2000_2019, "[[", 1))

### 01/1980-12/2019
PREC_FNAMES_1980_2019 <- PREC_FNAMES_2000_2019[c(2, 4:8, 10:14)]
PREC_FNAMES_SHORT_1980_2019 <- PREC_FNAMES_SHORT_2000_2019[c(2, 4:9, 11:14)] # DANGER: changes in order due to sort()

### 01/1960-12/2019
PREC_FNAMES_1960_2019 <- PREC_FNAMES_1980_2019[c(3:5, 8:9, 11)]
PREC_FNAMES_SHORT_1960_2019 <- PREC_FNAMES_SHORT_1980_2019[c(3:6, 10:11)]

save(PREC_FNAMES_2000_2019, PREC_FNAMES_SHORT_2000_2019, 
     PREC_FNAMES_1980_2019, PREC_FNAMES_SHORT_1980_2019,
     PREC_FNAMES_1960_2019, PREC_FNAMES_SHORT_1960_2019, file = '~/shared/data_projects/ithaca/misc/common_periods.Rdata')
