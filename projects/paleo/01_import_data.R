## Imports data from csv table and stores them as rds files.
source('source/paleo.R')

# recon_summary <- read.csv(paste0(PATH_SAVE_PALEO, "/mnt/shared/data/paleo/processed/pratap_thesis/recon_meta.csv"))
# recon_data_xls <- readxl::read_xls(paste0(PATH_SAVE_PALEO, "/mnt/shared/data/paleo/processed/pratap_thesis/recon_data.xls"))

recon_summary <- read.csv("/mnt/shared/data/paleo/processed/pratap_thesis/recon_meta.csv")
recon_data <- read.csv("/mnt/shared/data/paleo/processed/pratap_thesis/recon_data.csv")

### Assign recon_id ####
library(stringr)
library(dplyr)
# Add first four letters in recon_id from selected columns
# year from investigators 
extract_year <- function(investigators) {
  sapply(str_extract_all(investigators, "\\d{4}"), toString)
}

# From each rest of element
first_four <- function(x) {
  substr(x, 1, 4)
}


data_id <- recon_summary %>%
  mutate(loc_part = first_four(location),
         #frac_part = first_four(fractions),
         var_part = first_four(proxy),
         year_part = extract_year(investigators),
         recon_id = paste(loc_part, year_part, var_part, sep = "_") #frac_part, var_part, 
  ) %>%
  dplyr::select(recon_id)

head(data_id)

# Add recon_id to meta data
recon_summary <- cbind(data_id, recon_summary) %>%
  dplyr::select(-investigators)%>%
  filter(time_step != "varying")

recon_summary


### Add recon_id in main reconstruction data ###
additional_columns <- recon_summary %>%
  dplyr::select(recon_id, location)

final_recon_data <- recon_data %>%
  left_join(additional_columns, by = "location")

######################

final_recon <- final_recon_data %>%
  filter(time >= 1959 & time <= 2019) %>%
  dplyr::select(recon_id, time, value)  %>%
  mutate(value = round(value, 3))

final_recon

############

# save(recon_summary, paste0(PATH_SAVE_PALEO, "recon_summary.Rdata"))
# saveRDS(recon_data, paste0(PATH_SAVE_PALEO, "recon_data.rds"))

save(recon_summary, file = "~/shared/data_projects/ithaca/paleo/recon_summary.Rdata")
saveRDS(final_recon, file = "~/shared/data_projects/ithaca/paleo/recon_data.rds")





### Analysis #####
# load("~/shared/data_projects/ithaca/paleo/recon_summary.Rdata")

final_recon_rds <- readRDS(file = "~/shared/data_projects/ithaca/paleo/recon_data.rds")

filtered_recon <- final_recon_rds %>%
  filter(recon_id %in% c("Finn_2014_TRW", "Alps_2016_TRW", "Nort_2014_TRW"))


results <- filtered_recon %>%
  group_by(recon_id) %>%
  dplyr::summarize(
    mean_value = mean(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE),
    cv = sd_value / mean_value,
    slope = coef(lm(value ~ time))[2]
  )

results

