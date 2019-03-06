impute_missings_for_combat <- function(data, metabolites, batch) {
  
  # data <- b3.clean  # debug and testing
  # metabolites <- batch.metabolites # debug and testing
  # batch <- "FileName"

  # make copy of data to return later as to not change the input data (:= does the changes, but I dont want that)
  data.dummy <- copy(data)
  
  # NA postitions --------------------------------------------------------
  na.positions <- data.dummy[ , lapply(.SD, is.na), .SDcols = metabolites]

  # First: Use within-batch mean if possible ------------------------------
  data.dummy[ ,(metabolites) := lapply(.SD, function(x) {

    # replace NAs with the mean
    replace(x, is.na(x), mean(x, na.rm = TRUE))

    # The by-grouping assures that only mean within batch is used for replacement
  }), .SDcols = c(metabolites), by = eval((batch))]
  
  # Third: Use metabolite mean as last ressort -----------------
  data.dummy[ ,(metabolites) := lapply(.SD, function(x) {
    
    # replace NAs with the mean
    replace(x, is.na(x), mean(x, na.rm = TRUE))
  }), .SDcols = metabolites]
  
  # I want to return the imputed data as well as the positions of each NA to restore after ComBat
  output <- list("imputed.values" = data.dummy,
                 "na.positions" = na.positions)
  
  return(output)
}