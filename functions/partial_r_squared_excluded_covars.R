partial_r_squared_excluded_covars <- function(cohort, 
                              responses,
                              predictors,
                              data,
                              rSquaredCutoff,
                              getPValue,
                              verbose = T) {
  
  # debug/example
  # cohort <- "a1"
  # predictors <- reshaped.data[!is.na(a1.bonferroni), term]
  # responses <- unique(my.data$metab)
  # data <- metab.covar
  
  # iterativer vorgang: erst r2 bilden, dann das klein
  
  
  # rename for historical compatibility
  my.cohort <- cohort
  all.covars <- predictors
  all.metabs <- responses
  
  # middle loop over all responses, LHS etc
  r.squared.all.metabs <- lapply(all.metabs, function(my.metab) {
    
    # inner loop over all predictors
    r.squared.one.metab <- lapply(all.covars[all.covars == getPValue], function(my.covar){
      
      # start by checking for singular predictors
      my.test.formula <- as.formula(
        paste0(
          my.metab, " ~ ",
          paste(all.covars,
                collapse = " + ")))
      mm <- model.matrix(my.test.formula, data[cohort == my.cohort, ])
      sing.pred <- apply(mm,2,uniqueN)
      sing.pred <- sing.pred[names(sing.pred) != "(Intercept)"]
      
      # remove single integer in case of contrast
      names(sing.pred) <- gsub("\\d{1}$", replacement = "", names(sing.pred))
      remove.sing <- names(sing.pred[sing.pred<=1])
      all.covars <- all.covars[!(all.covars %in% remove.sing)]
      
      # my.metab <- all.metabs[1] # debug 
      # my.covar <- all.covars[1] # debug 

      # start by building the full and minus one model
      my.full.formula <- as.formula(paste0(my.metab, " ~ ", paste(all.covars, collapse = " + ")))
      my.reduced.formula <- as.formula(paste0(my.metab, " ~ 1",
                                              ifelse(length(all.covars) == 1, "", " + "),
                                              paste(all.covars[which(all.covars != my.covar)], collapse = " + ")))
      
      # fit the two models
      my.full.model <- summary(lm(formula = my.full.formula, data = data[cohort == my.cohort, ]))
      my.reduced.model <- summary(lm(formula = my.reduced.formula, data = data[cohort == my.cohort, ]))
      
      # extract the explained variance 
      my.full.adj.r.squared <- my.full.model$adj.r.squared
      my.full.r.squared <- my.full.model$adj.r.squared
      my.reduced.r.squared <- my.reduced.model$adj.r.squared
      
      # substract r-squares to get explained variance of my.covar
      my.r.squared <- my.full.r.squared - my.reduced.r.squared
      my.r.squared[my.r.squared < 0] <- 0
      
      my.coef <- as.data.frame(coefficients(my.full.model))
      setDT(my.coef, keep.rownames = T)
      my.rn <- grep(value = T, (getPValue), my.coef$rn)
      my.p.value <- my.coef[rn %in% (my.rn), min(`Pr(>|t|)`)]
      my.beta <- my.coef[rn %in% (my.rn), (Estimate)]
      my.beta <- my.beta[abs(my.beta) == max(abs(my.beta))]
      my.std.error <- my.coef[rn %in% (my.rn), max(`Std. Error`)]
      my.n <- length(my.full.model$residuals)
      
      res <- data.table(cohort = my.cohort,
                        response = my.metab,
                        term = my.covar,
                        estimate = my.beta,
                        std.error = my.std.error,
                        p.val = my.p.value,
                        term.r.squared = my.r.squared,
                        model.r.squared = my.full.adj.r.squared,
                        n = my.n,
                        comment = NA)
      
      if(length(remove.sing)!=0){
        res[,comment := paste0(
          "Singular predictor(s) ", 
          paste(remove.sing, collapse = ", "), 
          " removed from model! Consider removing factors with high missingness prior to analysis!")]
      }
      
      # return value
      return(res)
    }) # end of inner loop over all predictors
    
    # format for output, one col per predictor, one row per response
    my.output <- rbindlist(r.squared.one.metab)
    
    if(verbose == T) {
      # return to parent function
      message(paste(my.metab), " is done!")
    }
    
    return(my.output)
  }) # end of middle loop over all responses
  
  # format output
  r.squared.all.metabs <- rbindlist(r.squared.all.metabs)
  
  my.output <- r.squared.all.metabs
  # add a metabolite column
  
  # return cohort output
  return(my.output)
  
} # end of function
