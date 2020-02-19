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
            my.metab, " ~ ",
            paste(all.covars,
                  collapse = " + ")))
        my.reduced.formula <- as.formula(
          paste0(
            my.metab, " ~ 1",
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
                neworder = c("cohort", "metab", "term", "estimate", "std.error", "statistic", 
                             "term.r.squared", "model.r.squared", "p.value", "n", "comment"))
    
    # add a metabolite column
    return(my.output)
        
  })
  
  
  # all.cohort.multivariable <- lapply(unique(data$cohort), function(mycohort){
  #   
  #   # Testplan
  #   # mycohort <- "cohort_A"
  #   todo <- expand.grid(covar = docovar, metab = dometab, stringsAsFactors = F)
  #   setDT(todo)
  #   
  #   all_metab_covar_multi <- lapply(X = unique(todo$metab), function(mymetab){
  #     
  #     # mymetab <- unique(todo$metab)[1] # debug
  #     # define metabolite and confounders
  #     mycovars <- todo[metab == mymetab, covar]
  #     mydata <- data[cohort == (mycohort), ]
  #     
  #     # (generalized) linear Model with tryCatch
  #     res <- tryCatch(
  #       {
  #         #================================================================#
  #         # LINEAR MODEL --------------------------------------------------#
  #         
  #         # define the formula 
  #         myformula.lm <- as.formula(paste0(mymetab," ~ ", paste(mycovars, collapse = "+")))
  #         
  #         # Fit the linear model 
  #         mod.raw <- lm(myformula.lm, data = mydata)
  #         # mod.vif <- as.data.table(vif(mod.raw), keep.rownames = T)
  #         mod <- summary(mod.raw)
  #         
  #         # Clean the output
  #         # get the estimate from the raw model output because it includes all missings!
  #         coeffs.raw.lm <- as.data.frame(mod.raw$coefficients[
  #           grep(pattern = paste0(mycovars, collapse = "|"),
  #                x = names(mod.raw$coefficients), value = F)])
  #         
  #         # coeffs.raw.lm <- as.data.frame(mod.raw$coefficients[names(mod.raw$coefficients) %in% mycovars])
  #         setDT(coeffs.raw.lm, keep.rownames = T)
  #         setnames(coeffs.raw.lm, c("rn", "estimate"))
  #         
  #         # get the other statistiscs from the tidy output
  #         # coeffs.lm <- mod$coefficients[rownames(mod$coefficients) %in% mycovars, ]
  #         coeffs.lm <- mod$coefficients[grep(pattern = paste0(mycovars, collapse = "|"),
  #                                            x = names(mod.raw$coefficients), value = F), ]
  #         covar.order <- rownames(mod$coefficients)[grep(pattern = paste0(mycovars, collapse = "|"),
  #                                                        x = names(mod.raw$coefficients), value = F)]
  #         
  #         # stupid special case with only one covar 
  #         if(length(mycovars) == 1) {
  #           coeffs.raw.lm[ , `:=` (std.error.lm = coeffs.lm[which(names(coeffs.lm) == "Std. Error")],
  #                                  pval.lm = coeffs.lm[which(names(coeffs.lm) == "Pr(>|t|)")])]
  #         } else {
  #           
  #           coeffs.lm <- as.data.frame(coeffs.lm)
  #           setDT(coeffs.lm, keep.rownames = T)
  #           # match the coeffs coeffs.raw.lm
  #           coeffs.raw.lm[ , `:=` (std.error.lm = coeffs.lm[match(coeffs.raw.lm$rn, covar.order) , `Std. Error`],
  #                                  pval.lm = coeffs.lm[match(coeffs.raw.lm$rn, covar.order) , `Pr(>|t|)`])]
  #         }
  #         
  #         # remove double mentions from coeffs.raw.lm
  #         # check which coeffs.raw.lm$rn is mentioned more than once in coeffs.raw.lm and take the one with the smalles p-value
  #         exclude.rows <- sapply(mycovars, function(i){
  #           # i <- mycovars[6]
  #           
  #           # do not remove anything by default
  #           remove.this <- NULL
  #           db <- grep(pattern = paste0("^",i, ".+$"), x = coeffs.raw.lm$rn)
  #           
  #           if(length(db)!=0){
  #             # which of the factor results should I keep? -> dbi is the row to keep
  #             dbi <- coeffs.raw.lm[db, which(pval.lm == min(pval.lm))]
  #             keep.this <- db[dbi]
  #             remove.this <- db[db != keep.this]
  #           }
  #           return(remove.this)
  #         })
  #         
  #         # get all row indices to exclude
  #         exclude.rows <- unlist(exclude.rows)
  #         
  #         # filter rosw
  #         coeffs.raw.lm <- coeffs.raw.lm[!(exclude.rows), ]
  #         
  #         # change rownames of vif dt in case something is off again
  #         # if(ncol(mod.vif) == 2){
  #         #   names(mod.vif) <- c("rn", "GVIF")
  #         # }
  #         
  #         # Enter results into dt # 
  #         res <- data.table::data.table(cohort = mycohort,
  #                                       metab = mymetab,
  #                                       term = mycovars,
  #                                       estimate = coeffs.raw.lm$estimate,
  #                                       std.error = coeffs.raw.lm$std.error.lm,
  #                                       r.squared = mod$adj.r.squared,
  #                                       p.value = coeffs.raw.lm$pval.lm,
  #                                       # vif = mod.vif[match(rn, mycovars), GVIF],
  #                                       n = length(mod$residuals),
  #                                       comment = NA)
  #         
  #         # END OF: LINEAR MODEL ------------------------------------------#
  #         #================================================================#
  #       },
  #       error = function(cond){
  #         res <- data.table(cohort = mycohort,
  #                           metab = mymetab,
  #                           term = mycovars,
  #                           estimate = NA,
  #                           std.error = NA,
  #                           r.squared = NA,
  #                           p.value = NA, 
  #                           vif = NA,
  #                           n = NA,
  #                           comment = stringr::str_replace_all(as.character(cond), "\n", "  "))
  #         return(res)
  #       }
  #     ) # End of tryCatch()
  #     return(res)
  #   })
  #   
  #   all_metab_covar_multi <- rbindlist(all_metab_covar_multi)
  #   return(all_metab_covar_multi)
  #   
  #   
  #   
  # })
  
  # consolidate 
  res <- rbindlist(all.cohort.multivariable)
  return(res)
  
}
