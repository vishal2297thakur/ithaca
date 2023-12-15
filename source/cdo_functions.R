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
