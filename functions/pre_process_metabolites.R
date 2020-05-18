pre_process_metabolites <- function(
  preProcessMetabolites,
  preProcessingSteps,
  metaboliteAnnotation,
  dataObject,
  metaboliteColumns,
  covariateColumns
){
  
  # re-assign user input
  annot.m <- metaboliteAnnotation
  dat <- dataObject
  m.cols <- metaboliteColumns
  c.cols <- covariateColumns
  
  ###============###
  # Start analysis #
  ###============###
  if(!preProcessMetabolites){
    
    ### Center-Scale covariates ###
    
    # get column type
    c.unique <- dat[, sapply(.SD, is.factor), .SDcols = c.cols]
    
    # define categorical and continuous covariates
    c.continuous <- !c.unique
    
    # center scale ONLY the continuous covariates, the others are factors, which they need to be
    dat[, (c.cols[c.continuous]) := lapply(.SD, function(x){
      
      # center/scale
      x <- c(scale(x, center = T, scale = T))
      return(x)
    }),
    .SDcols = c.cols[c.continuous],
    by = cohort]
    
  return(list(dat = dat,
              annot.m = annot.m))
  } else{
    
    ###============###
    #     Filter     #
    ###============###
    
    
    # "Outlier Filter of 5*SD",
    if("Outlier Filter of 5*SD" %in% preProcessingSteps){
      
      message("Applying 5*SD Filter")
      
      # find data lost due to filtering
      filtered.samples.num <- dat[, .(n.reduction = sapply(
        .SD, data_reduction_by_filtering
      )),
      .SDcols = m.cols,
      by = cohort]
      
      # enter number of filtered samples into annotation
      annot.m[, filtered.samples := filtered.samples.num$n.reduction]
      
      # apply filter to data
      dat[, (m.cols) := lapply(.SD, function(x){
        sd_filter_vector(x)
        return(x)
      }), .SDcols = m.cols, by = cohort]
      
    } else {
      
      message("Applying No Filter")
      
      # add Info no filtering took place to the annotation
      annot.m[, filtered.samples := 0]
      
    }
    ###============###
    # Transformation #
    ###============###
    
    # "Inverse-Normal Transformation",
    if("Inverse-Normal Transformation" %in% preProcessingSteps){
      
      message("Inverse-Normal Transforming Data")
      
      # apply to data
      dat[, (m.cols) := lapply(.SD, function(x){
        x <- inverse_normal_transform(x)
        return(x)
      }), .SDcols = m.cols, by = cohort]
      
    } else{
      
      message("Not Transforming Data")
    
      } # skip if not selected
    
    ###============###
    # Batch effects  #
    ###============###
    ### Check for batch effects ###
    
    # iterate through the data (in each cohort)
    # bcbe <- before-combat batch effects
    
    if(length(unique(dat$batch))>1){
      
      bcbe <- dat[, .(kruskall.wallis.p = sapply(.SD, function(x){
        check_batch_effects(Metabolite = x,
                            Batch = batch)
      })),
      .SDcols = m.cols,
      by = cohort]
      
      # enter into annotation table
      annot.m[, k.w.p.value.before.Combat := bcbe$kruskall.wallis.p]
    } else {
      annot.m[, k.w.p.value.before.Combat := "no_batches"]
    }
    
    
    # "Batch Adjustment"
    if("Batch Adjustment" %in% preProcessingSteps){
      
      message("Batch Adjusting Data")
      
      ### Mean-impute data ###
      
      # filter for bad batches 
      # only impute good batches in order not to mix up resoration of NAs after Combat
      # find small batches and remove from adjustment
      bad.batches <- dat[ , .N, by = batch][N <= 1, batch]
      
      # message for removal of batches
      message(
        paste0(
          "Single-entry batches: ",
          paste(
            bad.batches,
            collapse = ", "
          ), " will be removed for reasons of no variance in this batch. (See sva::ComBat() documentation for explanation)."
        )
      )
      
      # remove single-entry batches from data
      dat <- dat[!(batch %in% (bad.batches)), ]
      
      message("Data will be mean-imputed (within batch) before batch adjustment.")
      
      # data imputation
      imputed.dat <- impute_missings_for_combat(
        data = dat,
        metabolites = m.cols,
        batch = "batch")
      
      # replace imputed values in clean dt
      dat[, (m.cols) := imputed.dat$imputed.values[, .SD, .SDcols = (m.cols)]]
      
      ### Batch adjustment ### 
      
      # perform batch adjustment per cohort
      expand.grid(cohort = unique(dat$cohort),
                  stringsAsFactors = F)
      
      # seperately for each cohort
      dat.batch <- lapply(unique(dat$cohort), function(x){
        
        # # exclude metabolites with no variance in batches
        # exclude.metabs <- batch.problem[
        #   found.problem == T &
        #     cohort %in% (x),
        #   unique(Metabolite)]
        # batch adjustment
        dat.batch.adj <- batch_adjustment(dat = dat[cohort %in% (x), ],
                                          batch = "batch",
                                          parameters = m.cols,
                                          my.par.prior = F)
        
        # identify failed columns (all NaN)
        failed.cols <- unlist(dat.batch.adj[, lapply(.SD, function(x)all(is.nan(x))), .SDcols = m.cols])
        failed.metabs <- names(failed.cols[failed.cols==T])
        
        # document batch adjustment method
        annot.m[cohort %in% x, batch.adjustment.in := "sva_ComBat"]
        annot.m[cohort %in% x &
                  metabolite %in% failed.metabs,
                batch.adjustment.in := "linear_model"]
        
        # # warning due to exclusion of metabolites 
        if(length(failed.metabs) != 0){
          
          # warning for failed adjustment
          message(paste0("Batch adjustment in ComBat failed for the metabolites ",
                         paste(failed.metabs, collapse = ", "),
                         ". They will be adjusted in a mixed linear model."))
          
          # warning(paste0("The metabolites",
          #                paste(exclude.metabs, collapse = ", "),
          #                " will be batch adjusted in a linear model due to low variance."))
          
          # batch adjust non-adjustable metabolites in a linear model
          # loop through non-ComBat-adjustable batches and adjust in LM
          lapply(failed.metabs, function(y){
            
            # y <- failed.metabs[1]
            # metab.formula <- as.formula(paste0(y, " ~ batch"))
            # metab.lm <- lm(metab.formula, data = dat[cohort %in% (x), ])
            
            # 180520: Use lmer instead of lm for manual batch adjustment
            metab.formula <- as.formula(paste0(y, " ~ 1 + (1 | batch)"))
            metab.lm <- lmer(metab.formula, data = dat[cohort %in% (x), ])
            
            batch.effects <- as.data.table(coef(metab.lm)$batch,keep.rownames = T)
            names(batch.effects) <- c("batch","intercept")
            dat.adj <- dat[cohort %in% (x) , .SD, .SDcols = c("batch", y)]
            names(dat.adj)[2] <- "metab"
            dat.adj[,metab.adj:= metab - batch.effects[match(dat.adj$batch, batch.effects$batch), intercept]]
            
            # transform residuals back into right scale
            # res.1 <- metab.lm$residual
            # res.2 <- dat[ , mean(unlist(.SD)), .SDcols = y]  + res.1
            
            # re-enter data into main data object
            set(x = dat.batch.adj, j = y, value = dat.adj$metab.adj)
          })
          message("Done!")
        } # end lm batch adjustment
        
        # return data 
        return(dat.batch.adj)
      })
      
      # enter batch adjusted data into dat
      dat <- rbindlist(dat.batch)
      
      ### Restore Missings ###
      
      dat <- restore_missings_after_combat(
        data = dat,
        metabolites = m.cols,
        na_positions = imputed.dat$na.positions)
      
      ### Check for batch-effects after adjustment ###
      
      # acbe <- after-combat batch effects
      acbe <- dat[, .(kruskall.wallis.p = sapply(.SD, function(x){
        check_batch_effects(Metabolite = x,
                            Batch = batch)
      })),
      .SDcols = m.cols,
      by = cohort]
      
      # enter into annotation table
      annot.m[, k.w.p.value.after.Combat := acbe$kruskall.wallis.p]
    
    } else {
      
      message("No Batch Adjustment")
      annot.m[, batch.adjustment.in := "None"]
      
    }
    
    ### Center-Scale covariates ###
    
    # get column type
    c.unique <- dat[, sapply(.SD, is.factor), .SDcols = c.cols]
    
    # define categorical and continuous covariates
    c.continuous <- !c.unique
    
    # center scale ONLY the continuous covariates, the others are factors, which they need to be
    dat[, (c.cols[c.continuous]) := lapply(.SD, function(x){
      
      # center/scale
      x <- c(scale(x, center = T, scale = T))
      return(x)
    }),
    .SDcols = c.cols[c.continuous],
    by = cohort]
    
    
    ###=====###
    # Results #
    ###=====###
    
    return(list(dat = dat,
                annot.m = annot.m))
    
  } # end if statement
} # end of functino