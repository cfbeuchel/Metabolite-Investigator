# set metab filter
sd_filter_vector <- function(x) {
  
  # center and scale -> get z-score
  log.x <- suppressWarnings(log(x))
  log.x[log.x == -Inf] <- NA
  scaled.x <- scale(log.x, center = T, scale = T)
  
  # set the filter
  filter.ok <- ifelse((scaled.x <= 5) | is.na(scaled.x), T, F)
  
  # in case of NA set filter to F
  filter.ok[is.na(filter.ok)] <- F
  
  # set all filtered entries of x to NA
  x[!filter.ok] <- NA
  
  return(x)
}
