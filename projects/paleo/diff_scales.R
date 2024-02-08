source("source/main.R")

install.packages('csa')
install.packages('HKprocess')
library(csa)
library(HKprocess)

dummy <- setDT(read.csv('~/shared/data/paleo/processed/ljn_hydro_proxy/Coo13.txt', sep = ""))

ggplot(dummy, aes(year, value)) +
  geom_line() + 
  theme_linedraw()

csa(as.numeric(dummy$value), threshold = 10, fast = TRUE, wn = TRUE)
mleHK(as.numeric(dummy$value))

###########################################################################

# Function to calculate pairwise differences
calculate_pairwise_diff <- function(data) {
  num_rows <- nrow(data)
  difference <- matrix(NA, nrow = num_rows, ncol = num_rows)
  diff_labels <- matrix(NA, nrow = num_rows, ncol = num_rows)
  for (i in 1:num_rows) {
    for (j in 1:num_rows) {
      difference[i, j] <- as.numeric(data[j] - data[i])
      diff_labels[i, j]  <- as.numeric(data[i])
    }
  }
  return(cbind(time = diff_labels[upper.tri(diff_labels)], difference = difference[upper.tri(difference)]))
}

all_diffs_x <- calculate_pairwise_diff(dummy[, .(value = year)])
all_diffs_y <- calculate_pairwise_diff(dummy[, .(value = value)])

all_diffs <- data.table(all_diffs_x, std_value_diff = all_diffs_y[, 2])

all_diffs[, time_step := cut(difference, c(0:10, seq(20, 100, 10), seq(200, 1000, 100)))]
all_diffs <- all_diffs[order(time_step)]

all_diffs[, max(abs(std_value_diff)), time_step]
all_diffs[, quantile(abs(std_value_diff), 0.9), time_step]
all_diffs[, .N, time_step]

ggplot(all_diffs, aes(difference, value)) +
  geom_point()

ggplot(all_diffs,aes(abs(std_value_diff), fill = time_step)) +
  geom_boxplot() + 
  coord_flip() +
  theme_bw()

#Do the same for the 10 aggregated time series at all possible 10-year means

