data_reduction_by_filtering <- function(x) {
  
  if(identical(find(what = "sd_filter_vector"), character(0))) {
    stop("This function needs the function sd_filter_vector() to work.\nPlease load it from the folder /functions/")
  }
  
  # get the nas in the vector
  old.nas <- sum(is.na(x))
  
  # filter and get new amount of NAs in vector
  new.nas <- sum(is.na(sd_filter_vector(x)))
  
  na.by.filter <- new.nas - old.nas
  
  return(na.by.filter)
}