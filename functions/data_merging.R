data_merging <- function(
  covariateData,
  covariateID,
  covariateColumns,
  metaboliteData,
  metaboliteID,
  metaboliteColumns,
  cohortID,
  batchID) {
  
  # define covariate & metabolite ID column to merge by
  covar.id <- covariateID #USER_INPUT#
  metab.id <- metaboliteID #USER_INPUT#
  input.covar <- covariateData #USER_INPUT#
  input.metab <- metaboliteData #USER_INPUT#
  cohort.col <- cohortID #USER_INPUT#
  batch.col <- batchID #USER_INPUT#
  m.cols <- metaboliteColumns #USER_INPUT#
  c.cols <- covariateColumns #USER_INPUT#
  
  # turn into DT in case it isn't one
  setDT(input.covar)
  setDT(input.metab)
  
  # create new unified ID column
  setnames(input.covar, old = covar.id, new = "id", skip_absent = T)
  setnames(input.metab, old = metab.id, new = "id", skip_absent = T)
  
  # merge my seperate data into test data
  id.overlap <- intersect(input.covar$id, input.metab$id)
  merge.message <- paste0("Matching unique ID overlap for ", uniqueN(id.overlap), " samples found in metabolite and covariate data.")
  m.ordered <- match(id.overlap, input.metab$id)
  c.ordered <- match(id.overlap, input.covar$id)
  
  # merge into single data object
  dat <- cbind(input.covar[c.ordered, ], 
               input.metab[m.ordered, ])
  
  # remove duplicate id colum
  dat <- dat[, .SD, .SDcols = unique(names(dat))]
  
  
  # user specified columns for id, cohort and batch
  old.names <- c(cohort.col,
                 batch.col)
  
  # get a copy of the given data
  setnames(x   = dat,
           old = old.names,
           new = c("cohort",
                   "batch"), skip_absent = T)
  
  # only carry around necessary data
  dat <- dat[, .SD, .SDcols = c("cohort", "batch", "id", c.cols, m.cols)]
  
  # Which covariates are continuous and which categorical
  # categocical based on number of unique values
  c.unique <- dat[, sapply(.SD, function(x){
    y <- length(unique(x))
    y <= 5
  }), .SDcols = c.cols]
  
  # define categorical and continuous covariates
  c.categorical <- c.unique
  c.continuous <- !c.categorical
  
  # set column type
  # factor for categorical variables
  # numeric for continuous variables
  dat[, (c.cols[c.categorical]) := lapply(.SD, as.factor), .SDcols = c.cols[c.categorical]]
  dat[, (c.cols[c.continuous])  := lapply(.SD, as.numeric), .SDcols = c.cols[c.continuous]]
  
  # check for only NA columns and remove them with a warning!
  na.factors <- sapply(c.cols, function(i){
    all(is.na(dat[[i]]))
  })
  
  if(any(na.factors==TRUE)){
    
    dat[, (names(na.factors)[na.factors==TRUE]) := NULL]
    dat[, (names(na.factors)[na.factors==TRUE]) := c(
      "Too many unique factors. Model is unsuitable for this type of data. Factor will be removed.",
      rep(NA, times= .N-1))]
    
    
    merge.message <- paste0(merge.message, " The factors ", paste(names(na.factors)[na.factors==TRUE], collapse = ", "), " have too many levels and are removed from further analysis.")
  }
  
  # return results
  return(list(dat = dat,
              message = merge.message,
              na.cols = na.factors))
}
