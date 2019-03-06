relevance_backwards_selection <- function(rSquaredCutoff,
                                          rawData,
                                          allResponses,
                                          covariateAnnotation,
                                          allPredictors,
                                          forceInclude = NA,
                                          InclHighMissings = T,
                                          highMissingsCutoff = NA){
  
  annot.c <- covariateAnnotation
  
  if(InclHighMissings == T){
    
    # change nothing
    hm <- NA
    
  } else if(InclHighMissings == F) {
    
    # hm = high missings
    if(is.na(highMissingsCutoff)){
      stop("Please select a missingness cutoff (highMissingsCutoff)")
    }
    
    # find high missing covariates in the annotation table
    hm <- annot.c[missings.relative >= highMissingsCutoff, unique(covariate)]
    
  } else {
    stop("You need to choose whether to keep or remove high missing variables.")
  }
  
  # start with all covars - matching with reshaped.data in the function call chooses the right covariates for each cohort
  all.terms <- allPredictors[!(allPredictors %in% hm)]
  
  # create a seperate list for each cohort (selection is done within each cohort)
  # create list with vector for each cohort and all terms
  all.terms.list <- sapply(unique(rawData$cohort), function(x){
    
    # atl = all terms list
    atl <- list(all.terms)
    names(atl) <- x
    return(atl)
  }, USE.NAMES = F)

  # prime the repeat loops with all confounders
  new.confounders.list <- all.terms.list
  
  # collect cohort-based results
  my.collection <- NULL
  
  # start index for iterations
  my.iter <- 1
  
  # repeat the following code, i.e. the partial r-squared computation and elimination of covariates that do not have at least one 5% r-squared hit
  repeat{
    
    # filter for old excluded confounders
    all.terms.list <- mapply(all.terms.list,
                             new.confounders.list,
                             FUN = function(x, y){x[x %in% y]},
                             SIMPLIFY = F)
    
    # Cohort-specific partial-r-squared calculation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
    
    ###################
    #loop through each cohort
    res <- lapply(names(all.terms.list), function(x){
      
      # select cohort specific terms for use in r2 calculation
      terms <- all.terms.list[[x]]
      
      # predictors partial r squared
      
      if(!length(terms) == 0){
      res <- partial_r_squared(
        cohort = x,
        predictors = terms,
        responses = allResponses,
        data = rawData,
        rSquaredCutoff = rSquaredCutoff,
        verbose = F)
      } else {
        res <- data.table(cohort = x,
                          metab = allResponses,
                          term = NA,
                          estimate = NA,
                          std.error = NA,
                          statistic = NA,
                          term.r.squared = NA,
                          model.r.squared = NA,
                          p.value = NA,
                          n = NA,
                          r.squared.cutoff = rSquaredCutoff)
      }
    })
    
    # bin results together
    all.res <- rbindlist(res, use.names = T, fill = T)

    # exclude unimportant covariates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
    
    # define old/current confounders
    old.confounders.list <- new.confounders.list
    
    # get the max r2 for each term in each cohort
    melt.max.res <- all.res[ , .(max.r.squared = max(term.r.squared, na.rm = T)), by = .(cohort, term)]
    
    # change -Inf to NA
    change_in_dt(dat = melt.max.res, from = -Inf, to = NA, change_in_dat = T)
    
    # get the smalles of the maxima
    # edit 190220 - Remove the forceInclude covars from the filter object 
    # this way, they will always be included in the final selection
    melt.max.min.res <- melt.max.res[
      !(term %in% forceInclude),
      .SD[which.min(max.r.squared)],
      by = cohort]
    
    # pretty things up a little bit
    message("Finished iteration: ", my.iter, " ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    
    # add to iteration
    my.iter <- my.iter + 1

    if(any(melt.max.min.res$max.r.squared < rSquaredCutoff)) {
      
      # check each list element for cutoff criteria and exclude covariate depending on criteria
      new.confounders.list <- sapply(melt.max.min.res$cohort, function(my.cohort){
        
        # get the old cohort specific confounders
        my.old <- old.confounders.list[[my.cohort]]
        
        if(melt.max.min.res[cohort == (my.cohort), max.r.squared < (rSquaredCutoff)]){
          
          # exclude covariate when r-squared is below cutoff
          my.new <- my.old[!(my.old %in% melt.max.min.res[cohort == (my.cohort), term])]
          
        } else {
          
          # no change when r-squared not below cutoff
          my.new <- my.old  
        }
        
        # output
        return(my.new)
        
      }, USE.NAMES = T, simplify = F) # END OF APPLY
      
    } else if(all(melt.max.min.res$max.r.squared >= rSquaredCutoff)) {
      # stop!
      break
      } else if(length(unlist(old.confounders.list))==0){
        
        # remove the last (non-relevant) covariate and return the result
        # stop in case no covariate meets the criterium
        break
        # stop("No covariate meets the cutoff criterium! Please select a lower r-squared cutoff.")
      }
  } # end of repeat loop
  
  # return the old.confounders variable (no reassignment as new.confounder when max.min >= 0.05)
  return(melt.max.res)
}