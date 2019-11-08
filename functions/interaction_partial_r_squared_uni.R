interaction_partial_r_squared_uni <- function(responses,
                              predictors,
                              dat,
                              verbose = T) {
  
  
  all.covars <- predictors
  all.metabs <- responses
  
  # testplan
  testplan <- expand.grid(resp = all.metabs,
                          pred = all.covars,
                          stringsAsFactors = F)
  setDT(testplan)
  testplan[, index := 1:.N]
  
  all.res <- lapply(testplan$index, function(i){
    
    my.metab <- testplan[index == (i), resp]
    my.covar <- testplan[index == (i), pred]
    
    # start by building the full and minus one model
    my.full.formula <- as.formula(
      paste0(
        my.metab, " ~ ", my.covar, "*cohort")
      )
      
    my.reduced.formula <- as.formula(
      paste0(
        my.metab, " ~ ",my.covar, " + cohort"))
    
    # fit the two models
    my.fm <- lm(formula = my.full.formula, data = dat)
    my.rm <- lm(formula = my.reduced.formula, data = dat)
    my.full.model <- summary(my.fm)
    my.reduced.model <- summary(my.rm)
    
    # logLik ratio test with p-val
    my.lrtest <- lmtest::lrtest(my.rm, my.fm)
    lrtest.p <- my.lrtest$`Pr(>Chisq)`
    lrtest.p <- lrtest.p[!is.na(lrtest.p)]
    
    # extract the explained variance 
    my.full.r.squared <- my.full.model$r.squared
    my.full.r.squared.adj <- my.full.model$adj.r.squared
    my.full.r.squared.adj[my.full.r.squared.adj < 0] <- 0
    my.reduced.r.squared <- my.reduced.model$r.squared
    
    # clean output
    res <- as.data.frame(coefficients(my.full.model))
    setDT(res, keep.rownames = T)
    setnames(res, old = names(res), new = c("term",
                                            "estimate",
                                            "std.error",
                                            "statistic",
                                            "interaction.p.value"))
    
    res[, lr.test.p := lrtest.p]
    
    # get the strongest interaction effect
    # in case of categorical, select the most significant one
    res.single <- res[term %in% grep(pattern = (paste0(my.covar, ".*cohort|cohort.*", my.covar)), x = res$term, value = T), ]
    
    if(nrow(res.single)!=1){
      res.single <- res[term %in% grep(pattern = (paste0(my.covar, ".*cohort|cohort.*", my.covar)), x = res$term, value = T), ]
      if(nrow(res)!=0){
        res.single <- res.single[interaction.p.value == min(interaction.p.value),]
      }
    }
    
    # substract r-squares to get explained variance of my.covar
    my.r.squared <- my.full.r.squared - my.reduced.r.squared
    my.r.squared[my.r.squared<0] <- 0
    
    # consolidate output
    res.single[, `:=`(metab = my.metab,
                      term = my.covar,
                      interaction.r.squared = my.r.squared,
                      model.r.squared = my.full.r.squared.adj,
                      n = length(my.full.model$residuals))]
    
    # return value
    return(res.single)
    
  })
  
  my.output <- rbindlist(all.res)
  
  # column order for easier readability
  setcolorder(x = my.output, 
              neworder = c("metab", "term", "estimate", "std.error", "statistic", 
                           "interaction.r.squared", "model.r.squared", "interaction.p.value", "lr.test.p", "n")
              )
  
  # return cohort output
  return(my.output)
  
} # end of function
