# Creates subsets that share common time period for each variable

##Precipitation

### 01/2000-12/2019
PREC_FNAMES_2000_2019_FULL_RECORD <- c(list.files(path = PATH_PREC_SIM, full.names = TRUE),
                           list.files(path = PATH_PREC_OBS, full.names = TRUE))
PREC_FNAMES_2000_2019_FULL_RECORD <- grep("land", PREC_FNAMES_2000_2019_FULL_RECORD, value = TRUE)[c(3, 7:8, 10:15, 17:22)]

dummy <- strsplit(PREC_FNAMES_2000_2019_FULL_RECORD, split = '//')
dummy <- sapply(dummy, "[[", 2)
dummy <- strsplit(dummy, split = '_')
PREC_FNAMES_SHORT_2000_2019 <- sapply(dummy, "[[", 1)

### 01/1980-12/2019
PREC_FNAMES_1980_2019_FULL_RECORD <- PREC_FNAMES_2000_2019_FULL_RECORD[c(2, 4:8, 10:14)]
PREC_FNAMES_SHORT_1980_2019 <- PREC_FNAMES_SHORT_2000_2019[c(2, 4:8, 11:14)] 

### 01/1960-12/2019
PREC_FNAMES_1960_2019_FULL_RECORD <- PREC_FNAMES_1980_2019_FULL_RECORD[c(3:5, 8:9, 11)]
PREC_FNAMES_SHORT_1960_2019 <- PREC_FNAMES_SHORT_1980_2019[c(3:5, 8:9, 11)]

save(PREC_FNAMES_2000_2019_FULL_RECORD, PREC_FNAMES_SHORT_2000_2019, 
     PREC_FNAMES_1980_2019_FULL_RECORD, PREC_FNAMES_SHORT_1980_2019,
     PREC_FNAMES_1960_2019_FULL_RECORD, PREC_FNAMES_SHORT_1960_2019, file = '~/shared/data_projects/ithaca/misc/common_periods.Rdata')
