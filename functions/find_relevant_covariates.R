find_relevant_covariates <- function(
  covariateAnnotation,
  metaboliteAnnotation,
  dataObject,
  rSquaredCutoff,
  includeHighMissings,
  missingnessCutoff,
  mandatoryInclusion,
  multipleTestingCorrection,
  metaboliteColumns,
  covariateColumns
){
  
  # re-assign
  annot.c <- covariateAnnotation
  annot.m <-  metaboliteAnnotation
  dat <- dataObject
  r.squared.cutoff <- rSquaredCutoff
  include.high.missings <- includeHighMissings
  missingness.cutoff <- missingnessCutoff
  mandatory.inclusion <- mandatoryInclusion
  m.cols <- metaboliteColumns
  c.cols <- covariateColumns
  
  
  ###=================###
  # Relevance selection #
  ###=================###
  
  # update annotation table with info on missingness exclusion
  if(!include.high.missings){
    hm <- annot.c[missings.relative>= (missingness.cutoff), unique(covariate)]
    annot.c[, paste0(missingness.cutoff*100, ".percent.missing.exclusion") := ifelse(covariate %in% hm, T, F)]
  } else {
    hm <- NA
    annot.c[, paste0(missingness.cutoff*100, ".percent.missing.exclusion") := F]
  }
  
  # create a column to mark mandatory inclusion
  annot.inclusion <- annot.c$covariate %in% (mandatory.inclusion)
  annot.c[, mandatory.inclusion := (annot.inclusion)]
  
  # loop through all r2 cutoffs selected
  all.pr2sel.res <- lapply(r.squared.cutoff, function(i){
    
    # calculation
    # i <- r.squared.cutoff[1]
    res <- relevance_backwards_selection(rSquaredCutoff = i,
                                         rawData = dat,
                                         allResponses = m.cols,
                                         allPredictors = c.cols,
                                         covariateAnnotation = annot.c,
                                         forceInclude = mandatory.inclusion,
                                         InclHighMissings = include.high.missings,
                                         highMissingsCutoff = missingness.cutoff)
    
    # consolidation
    res[ , r.squared.cutoff := (i)]
    
    # loop through cohorts and update annotation with results
    for(x in unique(res$cohort)){
      
      # relevant terms in this cohort
      rel.terms <- res[cohort == (x) & max.r.squared >= r.squared.cutoff, term]

      # enter into annotation table
      annot.c[cohort == (x), 
              paste0("relevant.at.", (i)*100, ".percent.r2") := 
                ifelse(covariate %in% (rel.terms), T, F)]
    }
    
    # return results
    return(res)
  })
  
  # bind before casting 
  pr2sel.res <- rbindlist(all.pr2sel.res)
  
  # print the model selection result
  final.res.paper <- pr2sel.res[
    r.squared.cutoff == (r.squared.cutoff),
    max.r.squared,
    by = .(cohort, term)]
  
  # create an empty result dt in case final.res.paper has nrow=0
  if(nrow(final.res.paper)==0){
    selection.res <- data.table(
      term = NA
    )
    new.cols <- unique(dat$cohort)
    selection.res[, (new.cols) := NA]
  } else{
    # cast for easier comparison
    selection.res <- dcast(final.res.paper, formula = term ~  cohort, value.var = "max.r.squared")
  }
  
  # source function to compute the partial r-squared
  final.pred <- selection.res$term
  
  # log the removed covars
  my.log <- c()
  
  # ================================================================
  # while there is no covar with r2 <= cutoff in all cohorts, do this:
  if(!all(is.na(final.pred))){
    
  repeat{
    
    # ========================
    # loop through each cohort
    all.partial <- lapply(
      unique(dat$cohort),
      function(my.cohort){
        
        # compute partial r2
        partial <- partial_r_squared(
          cohort = my.cohort,
          responses = m.cols,
          predictors = final.pred,
          data = dat,
          rSquaredCutoff = r.squared.cutoff,
          verbose = F
        )
        
        return(partial)
      })
    # loop through each cohort
    # ========================
    
    # consolidate
    partial.res <- rbindlist(all.partial)
    
    # get the max
    partial.max <- partial.res[
      , .(max.r.squared = max(term.r.squared, na.rm = T)),
      by = .(cohort, term)]
    
    # check for mins
    # nev - not enough variance
    nev <- partial.max[
      max.r.squared <= (r.squared.cutoff)]
    
    # lvc = low variance covariates in both cohorts?
    lvc <- nev[, .N, by = term][N == uniqueN(partial.max$cohort), term]
    
    if(length(lvc) == 0){
      message("All covariates explain enough variance in at least one cohort.")
      break}
    
    # get the mean to remove the weakest covar
    nev2 <- nev[term %in% lvc
                , .(mean = mean(max.r.squared, na.rm = T)),
                by = term
                ]
    
    # do not remove mandatory inclusion covars!
    cnr <- nev2[
      !(term %in% mandatory.inclusion),
      .(term = term[mean == min(mean)])
      ]
    
    # remove cnr -> i.e. the weakest predictor
    final.pred <- final.pred[!(final.pred %in% cnr$term)]
    
    # add logging event
    my.log <- append(x = my.log, values = cnr$term)
    
    # stop when nothing gets removed anymore
    if(length(cnr$term) == 0){
      message("All covariates explain enough variance in at least one cohort.")
      break}
  } 
  # end while loop 
  # ==============
  
  # now check the final partial-r2 selection -> is it the previous selection?
  # partial.res
  
  # get the total model r2 of each metabolite
  r2.total <- partial.res[, .(model.r.squared = unique(model.r.squared)), by = .(cohort, metab)]
  
  # enter the r2 of the relevant model for each metabolite
  # r2.total$metab == annot.m$metabolite
  # r2.total$cohort == annot.m$cohort
  # annot.m[ , relevant.model.r2 := r2.total$model.r.squared]
  
  # print some results
  message("Mean/Max explained variance per cohort")
  print(r2.total[, .(mean = mean(model.r.squared),
                     max = max(model.r.squared),
                     max.metab = metab[model.r.squared == max(model.r.squared)]), by = cohort])
  
  # cast
  full.model.r.squared <- partial.res[, .(max.r.squared = max(term.r.squared)), by = .(cohort, term)]
  full.model.r.squared <- dcast(full.model.r.squared, term ~ cohort, value.var = "max.r.squared")
  names(full.model.r.squared)[1] <- "covariate"
  
  # add to annotation table
  annot.c[, final.relevance := ifelse(covariate %in% full.model.r.squared$covariate, T, F)]
  
  message("These covariates were selected in each cohort:")
  # the final confounders selected were:
  print(full.model.r.squared)
  
  # rename and output only the necessary columns
  setnames(x = partial.res, old = "metab", new = "response", skip_absent = T)
  partial.res <- partial.res[ , .SD, .SDcols = setdiff(names(partial.res),
                                                      c("statistic"))]
  
  } else{ # end IF (empty final pred)
    full.model.r.squared <- data.table(covariate = NA)
    partial.res <- data.table(
      cohort = NA,
      response = NA,
      term = NA,
      estimate = NA,
      p.value = NA
    )
  } # end fail statement -> return empty results
  
  # return
  return(list(covariateAnnotation = annot.c,
              metaboliteAnnotation = annot.m,
              covariateModel = full.model.r.squared,
              testStatistics = partial.res,
              highMissings = hm))
}