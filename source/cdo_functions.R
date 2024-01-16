# cdo functions ----

## cdo info command ----
cdo_info_fnc <- function(inputfile_name){
  if(!is.character(inputfile_name)){
    stop(paste(inputfile_name, "is not a valid filename"))
  }
  
  file_extension <- strsplit(basename(inputfile_name), split="\\.")[[1]]
  if(file_extension[-1] != "nc"){
    stop(paste(basename(inputfile_name), "not a nc file"))
  }
  cmd_cdo <- paste("cdo info", inputfile_name)
  system(cmd_cdo)
}

## cdo sinfo ----
cdo_sinfo_fnc <- function(inputfile_name){
  if(!is.character(inputfile_name)){
    stop(paste(inputfile_name, "is not a valid filename"))
  }
  
  file_extension <- strsplit(basename(inputfile_name), split="\\.")[[1]]
  if(file_extension[-1] != "nc"){
    stop(paste(basename(inputfile_name), "not a nc file"))
  }
  cmd_cdo <- paste("cdo sinfo", inputfile_name)
  system(cmd_cdo)
}

## cdo multi-year monthly mean command ----
cdo_ymonmean_fnc <- function(inputfile_name, outputfile_name){
  if(!is.character(inputfile_name)){
    stop(paste(inputfile_name, "is not a valid filename"))
  }
  
  if(!is.character(outputfile_name)){
    stop(paste(outputfile_name, "is not a valid filename"))
  }
  
  file_extension <- strsplit(basename(inputfile_name), split="\\.")[[1]]
  if(file_extension[-1] != "nc"){
    stop(paste(basename(inputfile_name), "not a nc file"))
  }
  
  file_extension <- strsplit(basename(outputfile_name), split="\\.")[[1]]
  if(file_extension[-1] != "nc"){
    stop(paste(basename(outputfile_name), "not a nc file"))
  }
  cmd_cdo <- paste("cdo ymonmean", inputfile_name, outputfile_name)
  system(cmd_cdo)
}

## cdo year mean command ----
cdo_yearmean_fnc <- function(inputfile_name, outputfile_name){
  if(!is.character(inputfile_name)){
    stop(paste(inputfile_name, "is not a valid filename"))
  }
  
  if(!is.character(outputfile_name)){
    stop(paste(outputfile_name, "is not a valid filename"))
  }
  
  file_extension <- strsplit(basename(inputfile_name), split="\\.")[[1]]
  if(file_extension[-1] != "nc"){
    stop(paste(basename(inputfile_name), "not a nc file"))
  }
  
  file_extension <- strsplit(basename(outputfile_name), split="\\.")[[1]]
  if(file_extension[-1] != "nc"){
    stop(paste(basename(outputfile_name), "not a nc file"))
  }
  cmd_cdo <- paste("cdo yearmean", inputfile_name, outputfile_name)
  system(cmd_cdo)
}


## cdo field mean with weights command ----
cdo_fldmean_fnc <- function(inputfile_name, outputfile_name){
  if(!is.character(inputfile_name)){
    stop(paste(inputfile_name, "is not a valid filename"))
  }
  
  if(!is.character(outputfile_name)){
    stop(paste(outputfile_name, "is not a valid filename"))
  }
  
  file_extension <- strsplit(basename(inputfile_name), split="\\.")[[1]]
  if(file_extension[-1] != "nc"){
    stop(paste(basename(inputfile_name), "not a nc file"))
  }
  
  file_extension <- strsplit(basename(outputfile_name), split="\\.")[[1]]
  if(file_extension[-1] != "nc"){
    stop(paste(basename(outputfile_name), "not a nc file"))
  }
  cmd_cdo <- paste("cdo fldmean,weights=TRUE", inputfile_name, outputfile_name)
  system(cmd_cdo)
}



## cdo field sum with weights command ----
cdo_fldsum_fnc <- function(inputfile_name, outputfile_name){
  if(!is.character(inputfile_name)){
    stop(paste(inputfile_name, "is not a valid filename"))
  }
  
  if(!is.character(outputfile_name)){
    stop(paste(outputfile_name, "is not a valid filename"))
  }
  
  file_extension <- strsplit(basename(inputfile_name), split="\\.")[[1]]
  if(file_extension[-1] != "nc"){
    stop(paste(basename(inputfile_name), "not a nc file"))
  }
  
  file_extension <- strsplit(basename(outputfile_name), split="\\.")[[1]]
  if(file_extension[-1] != "nc"){
    stop(paste(basename(outputfile_name), "not a nc file"))
  }
  cmd_cdo <- paste("cdo fldsum", inputfile_name, outputfile_name)
  system(cmd_cdo)
}

## cdo select time ----
cdo_seltimestep_fnc <- function(inputfile_name, outputfile_name, start_time, end_time){
  if(!is.character(inputfile_name)){
    stop(paste(inputfile_name, "is not a valid filename"))
  }
  
  if(!is.character(outputfile_name)){
    stop(paste(outputfile_name, "is not a valid filename"))
  }
  
  file_extension <- strsplit(basename(inputfile_name), split="\\.")[[1]]
  if(file_extension[-1] != "nc"){
    stop(paste(basename(inputfile_name), "not a nc file"))
  }
  
  file_extension <- strsplit(basename(outputfile_name), split="\\.")[[1]]
  if(file_extension[-1] != "nc"){
    stop(paste(basename(outputfile_name), "not a nc file"))
  }
  
  if(!is.numeric(start_time) & !is.numeric(as.numeric(start_time))){
    stop(paste(start_time), "is not a number")
  }
  
  if(!is.numeric(end_time) & !is.numeric(as.numeric(end_time))){
    stop(paste(start_time), "is not a number")
  }
  
  cmd_cdo <- paste0("cdo seltimestep,",start_time,"/",end_time, " ",inputfile_name, " ", outputfile_name)
  system(cmd_cdo)
}

## cdo timemean
cdo_timmean_fnc <- function(inputfile_name, outputfile_name){
  if(!is.character(inputfile_name)){
    stop(paste(inputfile_name, "is not a valid filename"))
  }
  
  if(!is.character(outputfile_name)){
    stop(paste(outputfile_name, "is not a valid filename"))
  }
  
  file_extension <- strsplit(basename(inputfile_name), split="\\.")[[1]]
  if(file_extension[-1] != "nc"){
    stop(paste(basename(inputfile_name), "not a nc file"))
  }
  
  file_extension <- strsplit(basename(outputfile_name), split="\\.")[[1]]
  if(file_extension[-1] != "nc"){
    stop(paste(basename(outputfile_name), "not a nc file"))
  }
  cmd_cdo <- paste("cdo timmean", inputfile_name, outputfile_name)
  system(cmd_cdo)
}

## cdo standard deviation
cdo_timstd_fnc <- function(inputfile_name, outputfile_name){
  if(!is.character(inputfile_name)){
    stop(paste(inputfile_name, "is not a valid filename"))
  }
  
  if(!is.character(outputfile_name)){
    stop(paste(outputfile_name, "is not a valid filename"))
  }
  
  file_extension <- strsplit(basename(inputfile_name), split="\\.")[[1]]
  if(file_extension[-1] != "nc"){
    stop(paste(basename(inputfile_name), "not a nc file"))
  }
  
  file_extension <- strsplit(basename(outputfile_name), split="\\.")[[1]]
  if(file_extension[-1] != "nc"){
    stop(paste(basename(outputfile_name), "not a nc file"))
  }
  cmd_cdo <- paste("cdo timstd", inputfile_name, outputfile_name)
  system(cmd_cdo)
}

## cdo info to text
cdo_info_to_text_fnc <- function(inputfile_name, outputfile_name){
  if(!is.character(inputfile_name)){
    stop(paste(inputfile_name, "is not a valid filename"))
  }
  
  if(!is.character(outputfile_name)){
    stop(paste(outputfile_name, "is not a valid filename"))
  }
  
  file_extension <- strsplit(basename(inputfile_name), split="\\.")[[1]]
  if(file_extension[-1] != "nc"){
    stop(paste(basename(inputfile_name), "not a nc file"))
  }
  
  cmd_cdo <- paste("cdo info", inputfile_name, ">", outputfile_name)
  
  system(cmd_cdo)
  
  dummy <- read.table(outputfile_name, sep = "", header = F, skip = 1, flush = T)

  n_row <- nrow(dummy)-1

  dummy <- dummy[1:n_row,]
  
  date <- dummy$V3
  

  if(ncol(dummy) == 11){
    value <- as.numeric(dummy[,9])
  }
  
  if(ncol(dummy) == 13){
    value <- as.numeric(dummy[,10])
  }
  
  data <- data.table(date = as.Date(date), value = value)
  data <- data[complete.cases(data)]
  return(data)
}

cdo_change_field_fnc <- function(inputfile_name, outputfile_name, old_var_name, new_var_name, new_long_name, old_unit, new_unit){
  if(!is.character(inputfile_name)){
    stop(paste(inputfile_name, "is not a valid filename"))
  }
  
  if(!is.character(outputfile_name)){
    stop(paste(outputfile_name, "is not a valid filename"))
  }
  
  file_extension <- strsplit(basename(inputfile_name), split="\\.")[[1]]
  if(file_extension[-1] != "nc"){
    stop(paste(basename(inputfile_name), "not a nc file"))
  }
  
  file_extension <- strsplit(basename(outputfile_name), split="\\.")[[1]]
  if(file_extension[-1] != "nc"){
    stop(paste(basename(outputfile_name), "not a nc file"))
  }
  
  cdo_cmd <- paste0("cdo -chname,",old_var_name,",",new_var_name, " -chunit,",old_unit,",",new_unit," ", inputfile_name," ", outputfile_name)
  print(cdo_cmd)
  system(cdo_cmd)
  
  nc_cmd <- paste0("ncatted -a long_name,",new_var_name,",o,c,",new_long_name, " " , outputfile_name)
  system(nc_cmd)
  print(nc_cmd)
  print("Your new file attributes are")
  cmd_cdo <- paste0("cdo vardes ",outputfile_name)
  system(cmd_cdo)
}
