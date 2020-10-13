change_in_dt <- function(dat, columns = NA, from, to, stop_missing = TRUE, change_in_dat = FALSE) {
  
  # check 1
  if(missing(dat)){
    stop("Please assign a data.table, a list or data.frame to 'dat'.")
  } else if(!data.table::is.data.table(dat)) {
    if(!is.data.frame(dat) | !is.list(dat)) {
      stop("'dat' is not a data.frame and cannot be converted into a data.table. Please reconsider your input into 'dat'.")
    } else {
      try(data.table::setDT(dat))
    }
  }
  
  # check 2
  if(missing(to) | missing(from) | missing(dat)) {
    stop("Please assign a value to 'to' and 'from' and a data.table 'dat'.\nNo default was set (e. g. NA) to prohibit accidental deletions or entries.")
  } else if(length(to) != 1 | length(from) != 1) {
    stop("'to' and 'from' each need to be of length one.")
  } else if(identical(from, to)) {
    message("'from' has the same value as 'to'. No changes will be made")
    return(dat)
  }
  
  # check 3
  if(length(columns) == 1 & any(is.na(columns))) {
    # change in all columns of data.table by setting columns to names(dat)
    columns <- names(dat)
  } else if(is.numeric(columns)){
    # change numeric column entry into character vector of column names
    columns <- names(dat)[columns]
    if(any(is.na(columns)) & stop_missing == T) {
      stop("Some of your column indexes were not found in 'dat'.\nThis stop is was executed due to you setting stop_missing = T")
    } else if(any(is.na(columns)) & stop_missing == F) {
      message("Some or one of your entries into 'columns' was not found in 'dat'. However, I will continue with the columns I've found.")
      columns <- columns[!is.na(columns)]
    }
  } else if(any(!is.element(columns, names(dat)))) {
    stop("Some of your column names in 'columns' are not found in 'dat'")
  }
  
  # check 4
  if(change_in_dat == FALSE) {
    # get a copy of dat because data.table changes in source. I don't want that
    dat.dummy <- data.table::copy(dat)
    
    # actual computation happens here
    if(is.na(from)) {
      # use is.na(from) for finding elememts that are to be changed
      dat.dummy[ , (columns) := lapply(.SD, function(x){
        x[is.na(x)] <- to
        return(x)
      }), .SDcols = columns][]
    } else if(is.numeric(from)){
      dat.dummy[ , (columns) := lapply(.SD, function(x){
        x[x == (from)] <- to
        return(x)
      }), .SDcols = columns][]
    } else {
      dat.dummy[ , (columns) := lapply(.SD, function(x){
        x[grep(pattern = paste0("^", from, "$"), x = x)] <- to
        return(x)
      }), .SDcols = columns][]
    }
    return(dat.dummy)
    
    # other variant
  } else if(change_in_dat == TRUE) {
    
    # actual computation happens here
    if(is.na(from)) {
      # use is.na(from) for finding elememts that are to be changed
      dat[ , (columns) := lapply(.SD, function(x){
        x[is.na(x)] <- to
        return(x)
      }), .SDcols = columns][]
    } else if(is.numeric(from)){
      dat[ , (columns) := lapply(.SD, function(x){
        x[x == (from)] <- to
        return(x)
      }), .SDcols = columns][]
    } else {
      dat[ , (columns) := lapply(.SD, function(x){
        x[grep(pattern = paste0("^", from, "$"), x = x)] <- to
        return(x)
      }), .SDcols = columns][]
    }
  }
}
