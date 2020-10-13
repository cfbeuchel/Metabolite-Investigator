multivariable_assoc <- function(dometab, docovar, data, r_on_server = F) {
  
  #=================================================================#
  # SETUP ----------------------------------------------------------#
  
  all.metabs <- dometab
  all.covars <- docovar
  
  all.cohort.multivariable <- lapply(unique(data$cohort), function(my.cohort){
    
    # middle loop over all responses, LHS etc
    r.squared.all.metabs <- lapply(all.metabs, function(my.metab) {
      
      # inner loop over all predictors
      r.squared.one.metab <- lapply(all.covars, function(my.covar){
        
        # my.metab <- all.metabs[1] # debug 
        # my.covar <- all.covars[5] # debug 
        # my.cohort <- unique(data$cohort)[1]
        
        # start by building the full and minus one model
        # get the full lm formulas
        my.full.formula <- as.formula(
          paste0(
            "`", my.metab, "`", " ~ ",
            paste(all.covars,
                  collapse = " + ")))
        my.reduced.formula <- as.formula(
          paste0(
            "`", my.metab, "`", " ~ 1",
            ifelse(length(all.covars) == 1, "", " + "),
            paste(all.covars[which(all.covars != my.covar)], collapse = " + ")))
        
        res.single <- tryCatch(
          {
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
            factor.term <- NA

            if(nrow(res.single)!=1){
              
              factor.term <- unique(data[cohort == (my.cohort), ][[my.covar]])
              res.single <- res[term %in% grep(pattern = (
                paste0("^", my.covar, "[",
                  paste(factor.term, collapse=","), "]"
                  )
                ), x = res$term, value = T), ]
              
              # save factor levels for results
              factor.term <-  unlist(grep(pattern = my.covar, res$term, value = T))
              factor.term <- gsub(pattern = my.covar, replacement = "", x = factor.term)
              
              all.levels <- paste(unique(data[[my.covar]]),collapse=", ")
              factor.term <- paste0(factor.term, " (Levels: ", all.levels, ")")
              
              # if(nrow(res)!=0){
              #   res.single <- res.single[p.value == min(p.value),]
              # }
            }
            
            # substract r-squares to get explained variance of my.covar
            my.r.squared <- my.full.r.squared - my.reduced.r.squared
            my.r.squared[my.r.squared < 0] <- 0
            
            
            # consolidate output
            res.single[, `:=`(cohort = my.cohort,
                              metab = my.metab,
                              term = my.covar,
                              factor.term = factor.term,
                              term.r.squared = my.r.squared,
                              model.r.squared = my.full.r.squared.adj,
                              n = length(my.full.model$residuals),
                              comment = NA)]
            
            # return value
            return(res.single)
            
          },error=function(cond){
            
            res.single <- data.table(
              term = my.covar,
              estimate = NA,
              std.error = NA,
              statistic = NA,
              p.value = NA,
              cohort = my.cohort,
              metab = my.metab,
              factor.term = NA,
              term.r.squared = NA,
              model.r.squared = NA,
              n = NA,
              comment = gsub(x = as.character(cond),
                             pattern = "\n",
                             replacement = " ")
            )
            
            # return better error message for contrast error
            if(grepl(pattern = "contrast",res.single$comment)){
              res.single[,comment := "Cannot fit model due to singular variable in data! Check NA structure and remove factors from analysis."]
              }
            
            return(res.single)
          }
        )
        
      }) # end of inner loop over all predictors
      
      my.output <- rbindlist(r.squared.one.metab)
      
      return(my.output)
    }) # end of middle loop over all responses
    
    # format output
    my.output <- rbindlist(r.squared.all.metabs)
    
    # column order for easier readability
    setcolorder(x = my.output, 
                neworder = c("cohort", "metab", "term","factor.term", "estimate", "std.error", "statistic", 
                             "term.r.squared", "model.r.squared", "p.value", "n", "comment"))
    
    # add a metabolite column
    return(my.output)
        
  })

  # consolidate 
  res <- rbindlist(all.cohort.multivariable)
  return(res)
  
}
