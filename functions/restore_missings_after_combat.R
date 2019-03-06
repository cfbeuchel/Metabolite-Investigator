restore_missings_after_combat <- function(data, metabolites, na_positions) {
  
  # set all values in data, whose entry in na_position, is TRUE, to NA
  # first create a copy of the data in order not to change the original file
  data.dummy <- copy(data)
  
  # map over both dts
  # res <- map2(.x = data.dummy[, ..metabolites],
  #      .y = na_positions,
  #      .f = function(x, y) {
  res <- mapply(x = data.dummy[, ..metabolites], y = na_positions, function(x,y) {
         x[y] <- NA
         x
       })
  
  # input in dummy
  res <- as.data.frame(res)
  setDT(res)
  data.dummy[, (metabolites) := res]
  
  # output
  return(data.dummy)
}