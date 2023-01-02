# Transform a brick object to data.table format

brick_to_dt <- function(x){
  x_dt <- x %>% as.data.frame(xy = TRUE, long = TRUE, na.rm = TRUE)
  x_dt <- as.data.table(x_dt)
  setnames(x_dt, colnames(x_dt)[3], 'time')
  x_dt[, time := as.Date(time)]
  return(x_dt)
}
