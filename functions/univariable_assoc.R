univariable_assoc <- function(dometab, docovar, data){
  
  # Testplan
  # Testplan
  todo <- expand.grid(covar = docovar,
                      metab = dometab,
                      cohort = unique(data$cohort),
                      stringsAsFactors = F)
  setDT(todo)
  todo[, index := 1:.N]
  
  # First check, whether single covariables are significantly associated with the metabolites
  
  #================================================================#
  # SINGLE COVARIABLES --------------------------------------------#
  all_metab_covar <- lapply(todo$index, function(i){
    
    # mycovar <- unique(subtodo$covar)[2]
    mycovar <- todo[index == (i), covar]
    mymetab <- todo[index == (i), metab]
    mycohort <- todo[index == (i), cohort]
    mydata <- data[cohort == (mycohort), ]
    
    
    res <- tryCatch(
      {
        #================================================================#
        # LINEAR MODEL --------------------------------------------------#
        
        # Fit the linear model 
        myformula.lm <- as.formula(paste0("`", mymetab, "`", " ~ ", mycovar))
        mod <- summary(lm(formula = myformula.lm, data = mydata))
        
        # Clean the output
        coeffs.lm <- as.data.frame(mod$coefficients)
        setDT(coeffs.lm, keep.rownames = T)
        estimate.lm <- unlist(coeffs.lm[rn %in% (grep(pattern = mycovar, coeffs.lm$rn, value = T)), "Estimate"])
        std.error.lm <- unlist(coeffs.lm[rn %in% (grep(pattern = mycovar, coeffs.lm$rn, value = T)), "Std. Error"])
        pval.lm <- unlist(coeffs.lm[rn %in% (grep(pattern = mycovar, coeffs.lm$rn, value = T)), "Pr(>|t|)"])
        
        # Enter results into dt
        res <- data.table::data.table(cohort = mycohort, 
                                      metab = mymetab,
                                      term = mycovar,
                                      estimate = estimate.lm,
                                      std.error = std.error.lm,
                                      r.squared = mod$adj.r.squared,
                                      p.value = pval.lm,
                                      n = length(mod$residuals),
                                      comment = NA)
        
        # END OF: LINEAR MODEL ------------------------------------------#
        #================================================================#
      },
      error = function(cond){
        res <- data.table(cohort = mycohort,
                          metab = mymetab,
                          term = mycovar,
                          estimate = NA,
                          std.error = NA,
                          r.squared = NA,
                          p.value = NA,
                          n = NA,
                          comment = gsub(x = as.character(cond),
                                         pattern = "\n",
                                         replacement = " "))
        return(res)
      }
    ) # End of tryCatch()
    
    # in case of factor, this will be longer than 1 -> only use the most important factor
    if(nrow(res) != 1){
      
      # get the result with the highest r²
      res <- res[which(p.value == min(p.value, na.rm = T)), ]
    }
    
    # return res to all_covar as a result of the lapply loop
    return(res)
    
  }) # End of covar-lapply
  
  # END OF: SINGLE COVARIABLES ------------------------------------#
  #================================================================#
  
  # Bind the final data.table together with all metabolites
  all_metab_covar <- rbindlist(all_metab_covar, use.names = T)
  
  #output
  return(all_metab_covar)
}
