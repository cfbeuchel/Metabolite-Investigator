remaining_covariates <- function(
  relevantCovariates,
  CovariateColumns,
  MetaboliteColumns,
  dataObject,
  rSquaredCutoff,
  highMissings
){
  
  # re-assign
  full.model.r.squared <- relevantCovariates
  c.cols <- CovariateColumns
  m.cols <- MetaboliteColumns
  dat <- dataObject
  r.squared.cutoff <- rSquaredCutoff
  hm <- highMissings
  
  # obligatory terms
  obl.terms <- full.model.r.squared$covariate
  
  # which are the terms for which I did not calculate the multivariable r-squrared distribution
  add.terms <- c.cols[!(c.cols %in% obl.terms)]
  add.terms <- add.terms[!(add.terms %in% hm)]
  
  # create a new multivariable model with all the selected covars including one of the additional terms and calculate their
  # i.e. i have length(add.terms) new models from which I only extract the r-squared distribution of add.terms[i]
  all.plans <- expand.grid(terms = add.terms,
                           cohort = unique(dat$cohort),
                           stringsAsFactors = F)
  setDT(all.plans)
  all.plans[, index := 1:.N]
  
  # loop through combinations
  add.r.squared <- lapply(all.plans$index, function(i){
    
    # i <- 4 # debug
    my.cohort <- all.plans[index == (i), cohort]
    my.add.term <- all.plans[index == (i), terms] # my additional term
    
    # get all covars 
    # ft = full terms
    ft <- c(my.add.term, obl.terms)
    ft <- ft[!is.na(ft)]
    
    # calc partial-r2
    res.partial <- partial_r_squared_excluded_covars(
      cohort = my.cohort,
      responses = m.cols,
      predictors = ft,
      data = dat,
      rSquaredCutoff = r.squared.cutoff,
      getPValue = my.add.term,
      verbose = F
    )
    
    # return
    return(res.partial)
  })
  
  # clean
  add.r.squared <-  rbindlist(add.r.squared, use.names = T, fill = T)
  setnames(x = add.r.squared, old = "p.val", new = "p.value", skip_absent = T)
  if(!all(dim(add.r.squared) == 0)){
  add.r.squared[, r.squared.cutoff := r.squared.cutoff]
  }
  
  # create empty dt when empty to avoid crash
  if(nrow(add.r.squared)==0){
  
    add.r.squared <- data.table(
      cohort = NA,
      response = NA,
      term = NA,
      estimate = NA,
      std.error = NA,
      term.r.squared = NA,
      model.r.squared = NA,
      p.value = NA,
      n = NA,
      r.squared.cutoff = r.squared.cutoff
    )
  }
  
  # return output
  return(add.r.squared)
}
