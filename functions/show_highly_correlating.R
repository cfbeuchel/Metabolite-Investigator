show_highly_correlating <- function(
  dataObject,
  covariateColumns,
  correlationCutoff
){
  
  # re-assign
  dat <- dataObject
  c.cols <- covariateColumns
  correlation.cutoff <- correlationCutoff
  
  # check correlation in each cohort
  all.high.corr <- sapply(unique(dat$cohort), function(x){
    
    # x <- unique(dat$cohort)[1]
    # correlation matrix
    M <- cor(
      dat[
        cohort == (x),
        lapply(.SD, as.numeric),
        .SDcols = c(c.cols)
        ], use = "pairwise"
    )
    
    # get all covariates correlating above the set threshold
    diag(M) <- 0
    M <- as.data.frame(M)
    setDT(M, keep.rownames = T)
    hc <- M[, lapply(.SD, function(i){
      abs(i) >= correlation.cutoff
    }), by = rn]
    hc[, high.corr := rowSums(.SD), by = rn]
    high.corr <- hc[high.corr!=0, rn]
    res <- list(high.corr)
    names(res) <- x
    return(res)
  }, USE.NAMES = F)
  
  # consolidate
  all.high.corr <- Reduce(union, all.high.corr)
  
  # get all high correlating covariates
  message(paste0("The covariates ", paste(all.high.corr, collapse = ", "),
                 " correlate above the specified threshold of ",
                 correlation.cutoff,
                 ". Please select those you wish to exclude from the analysis."))
  
  # output highly correlating variabes
  return(all.high.corr)
}
