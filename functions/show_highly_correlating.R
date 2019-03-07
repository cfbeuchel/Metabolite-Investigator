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
    
    # Plot correlation
    p.mat <- cor.mtest(dat[
      cohort == (x),
      lapply(.SD, as.numeric),
      .SDcols = c(c.cols)
      ])$p
    
    # col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
    # # plot
    # corrplot(M,
    #          method = "color",
    #          col = col(200),
    #          type = "upper",
    #          # order = "hclust",
    #          addCoef.col = "black", # Add coefficient of correlation
    #          tl.col = "black",
    #          title = paste0("Pairwise Pearsons' correlation of ", x, " metabolites."),
    #          tl.srt = 45, # Text label color and rotation
    #          # tl.pos = "d",
    #          p.mat = p.mat, # Combine with significance
    #          sig.level = 0.01,
    #          insig = "blank", # hide correlation coefficient on the principal diagonal
    #          diag = FALSE,
    #          mar = c(0,0,2,0)
    #          # number.cex = 15/nrow(M),
    #          # tl.cex = 20/nrow(M)
    # )
    
    # get all covariates correlating above the set threshold
    
    diag(M) <- 0
    M <- as.data.frame(M)
    setDT(M, keep.rownames = T)
    hc <- M[, lapply(.SD, function(i){
      i >= correlation.cutoff
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
