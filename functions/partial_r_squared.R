partial_r_squared <- function(cohort,
                              responses,
                              predictors,
                              data,
                              rSquaredCutoff,
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
    
    # inner loop over all predictors
    r.squared.one.metab <- lapply(all.covars, function(my.covar){
      
      # my.metab <- all.metabs[1] # debug 
      # my.covar <- all.covars[5] # debug 
      
      # start by building the full and minus one model
      my.full.formula <- as.formula(
        paste0(
          my.metab, " ~ ",
          paste(all.covars,
                collapse = " + ")))
      my.reduced.formula <- as.formula(
        paste0(
          my.metab, " ~ 1",
          ifelse(length(all.covars) == 1, "", " + "),
          paste(all.covars[which(all.covars != my.covar)], collapse = " + ")))
      
      # fit the two models
      my.full.model <- summary(lm(formula = my.full.formula, data = data[cohort == my.cohort, ]))
      my.reduced.model <- summary(lm(formula = my.reduced.formula, data = data[cohort == my.cohort, ]))
      
      # extract the explained variance 
      my.full.r.squared <- my.full.model$adj.r.squared
      my.full.r.squared.adj <- my.full.model$adj.r.squared
      my.reduced.r.squared <- my.reduced.model$adj.r.squared
      
      # clean output
      res <- as.data.frame(coefficients(my.full.model))
      setDT(res, keep.rownames = T)
      setnames(res, old = names(res), new = c("term",
                                              "estimate",
                                              "std.error",
                                              "statistic",
                                              "p.value"))

      # in case of categorical, select the most significant one
      res.single <- res[term %in% grep(pattern = (paste0("^", my.covar, "$")), x = res$term, value = T), ]
      
      if(nrow(res.single)!=1){
        res.single <- res[term %in% grep(pattern = (paste0("^",my.covar, ".+$")), x = res$term, value = T), ]
        if(nrow(res)!=0){
          res.single <- res.single[p.value == min(p.value),]
        }
      }

      # substract r-squares to get explained variance of my.covar
      my.r.squared <- my.full.r.squared - my.reduced.r.squared
      my.r.squared[my.r.squared < 0] <- 0
      
      # consolidate output
      res.single[, `:=`(cohort = my.cohort,
                 metab = my.metab,
                 term = my.covar,
                 term.r.squared = my.r.squared,
                 model.r.squared = my.full.r.squared.adj,
                 n = length(my.full.model$residuals),
                 comment = NA)]
      
      # add error in case of removed singular
      if(length(remove.sing)!=0){
        res.single[,comment := paste0(
          "Singular predictor(s) ", 
          paste(remove.sing, collapse = ", "), 
          " removed from model! Consider removing factors with high missingness prior to analysis!")]
      }
      
      # return value
      return(res.single)
    }) # end of inner loop over all predictors
    
    my.output <- rbindlist(r.squared.one.metab)
    
    if(verbose == T) {
      # return to parent function
      message(paste(my.metab), " is done!")
    }
    
    return(my.output)
  }) # end of middle loop over all responses
  
  # format output
  my.output <- rbindlist(r.squared.all.metabs)
  
  # column order for easier readability
  setcolorder(x = my.output, 
              neworder = c("cohort", "metab", "term", "estimate", "std.error", "statistic", 
                           "term.r.squared", "model.r.squared", "p.value", "n", "comment"))
  
  # add a metabolite column
  my.output[ , `:=`(r.squared.cutoff = rSquaredCutoff)]
  
  # return cohort output
  return(my.output)
  
} # end of function
