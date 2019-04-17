test_r2_distribution <- function(dat,
                                 r2Col){
  
  # change names
  dat2 <- copy(dat)
  setnames(dat2, r2Col, "r.squared", skip_absent = T)
  
  # how many cohorts do I have?
  cohorts <- unique(dat2$cohort)
  n.cohorts <- uniqueN(dat2$cohort)
  
  # format
  tmp1 <- dcast(
    data = dat2[, .(metab,
                   term = as.character(term),
                   cohort,
                   R2 = r.squared)],
    formula = as.formula(metab + term ~ cohort),
    value.var = "R2"
  )
  
  if(n.cohorts == 1){
    res <- NULL
  } else if(n.cohorts == 2){
    
    # wilcoxon test
    tmp2 <- lapply(unique(tmp1$term), function(x){
    
    dat.mat <- as.matrix(tmp1[term == (x), .SD ,.SDcols = cohorts])
    res <- wilcox.test(x = dat.mat[,1],
                       y = dat.mat[,2],
                       paired = T)
    res.out <- data.table(term = x,
                          wilcoxon.chi.sq = res$statistic,
                          wilcoxon.p.value = res$p.value
                          )
    
    return(res.out)
    })
    
    # consolidate results
    res.out <- rbindlist(tmp2)
    
    # do BH correction
    res.out[, wilcoxon.fdr := p.adjust(wilcoxon.p.value, method = "BH")]
    
  } else if(n.cohorts > 2){
    
    # friedman test
    tmp2 <- lapply(unique(tmp1$term), function(x){
      
    dat.mat <- as.matrix(tmp1[term == (x), ][order(term)][ ,.SD, .SDcols = cohorts])
    dat.blocks <- tmp1[term == (x), ][order(term)][ ,(term)]
    rownames(dat.mat) <- dat.blocks
    tmp2 <- friedman.test(dat.mat)
    tmp3 <- data.table(term = x,
                       friedman.chi.sq = tmp2$statistic,
                       friedman.p.value = tmp2$p.value
                       )
    return(tmp3)
    })
    
    # consolidate results
    res.out <- rbindlist(tmp2)
    
    # do BH correction
    res.out[, friedman.fdr := p.adjust(friedman.p.value, method = "BH")]
    
  } else stop("Something went wrong. Neither 1, 2 or more cohorts detected.")
  
  # output
  return(res.out)
}