parse_mztab <- function(mzraw = NULL){
  
  ### function to read a data set following the mzTab-M 2.0 format ###
  # PMID: 30688441 & https://github.com/HUPO-PSI/mzTab
  # Specifications: https://hupo-psi.github.io/mzTab/2_0-metabolomics-release/mzTab_format_specification_2_0-M_release.pdf
  
  # Read data blocks ----
  
  # all the different data blocks
  blocks <- c("mtd", "smh", "sml", "sfh", "smf", "seh", "sme")
  
  # get each block from the raw data
  mzdat <- lapply(blocks, function(i){
    
    # the text is marked by each block ID in upper case letters
    marker <- toupper(i)
    bloc.loc <- grep(pattern = paste0("^", marker), mzraw)
    
    if(length(bloc.loc)>0) {
      
      mzpart <- mzraw[bloc.loc]
      mzpart <- read.table(text = mzpart, sep="\t", header = F, fill = T, stringsAsFactors = F)
      setDT(mzpart)
      
      # remove all NA cols
      na.cols <- sapply(names(mzpart),function(i){
        all(is.na(mzpart[[i]]))
      })
      if(sum(na.cols)>0) {
        mzpart[, names(na.cols[na.cols==T]) := NULL]
      }
      
      # return
      return(mzpart)
      
    } else {
      
      return(NULL)
      
    }
  })
  
  # name the blocks (using the header when available)
  
  # meta data
  mtd <- mzdat[[1]]
  names(mtd) <- c("type", "parameter", "value")
  mtd$type <- NULL
  
  # sml - small molecule table (das hier sollte das wichtige sein)
  sml <- mzdat[[3]]
  names(sml) <- unlist(mzdat[[2]])
  
  # smf - small molecule feature table
  smf <- mzdat[[5]]
  names(smf) <- unlist(mzdat[[4]])
  
  # sme - small molecule evidence
  
  sme <- mzdat[[7]]
  names(sme) <- unlist(mzdat[[6]])

  # FORMAT ----
  
  # Format MTD data ----
  
  # assay name
  assay.name <- grep(pattern = "^assay\\[\\d+\\]$", x = mtd$parameter)
  
  # aggregate information
  assay.info <- data.table(
    assay = mtd[(assay.name), parameter],
    assay.val = mtd[(assay.name), value]
  )
  
  # match the correct sample & run references
  m.sample <- match(paste0(assay.info$assay, "-sample_ref"), mtd$parameter)
  assay.info[, sample := mtd[(m.sample), value]]
  
  # match the correct values
  m.sample <- match(assay.info$sample, mtd$parameter)
  assay.info[, sample.val := mtd[(m.sample), value]]
  
  # match the ms run 
  m.run <- match(paste0(assay.info$assay, "-ms_run_ref"), mtd$parameter)
  assay.info[, run := mtd[(m.run), value]]
  
  # add col for "abundance_assay[1-n] for easier merging
  assay.info[, abundance.assay := paste0("abundance_",assay)]
  
  # add tissue, cell_type and disease [1-n]
  # TODO ----
  
  # extract study_variable names (=covariates)
  covar.name <- grep(pattern = "^study\\_variable\\[\\d+\\]$", x = mtd$parameter, value = T)
  
  # aggregate information
  covar.info <- data.table(
    covar = covar.name,
    assay.name = mtd[parameter %in% (covar.name), value],
    assay.sample = mtd[parameter %in% paste0(covar.name, "-assay_refs"), value],
    assay.desc = mtd[parameter %in% paste0(covar.name, "-description"), value]
  )
  
  # expand covar Info for each sample
  covar.info.long <- lapply(covar.info$covar, function(i){
    tmp1 <- strsplit(x = covar.info[covar == (i), assay.sample], split = " | ", fixed = TRUE)[[1]]
    tmp1 <- gsub(pattern = " ",replacement = "",fixed = TRUE, x = tmp1)
    return(
      data.table(
        covar = (i),
        assay.name = covar.info[covar == (i), assay.name],
        assay.sample = tmp1,
        assay.desc = covar.info[covar == (i), assay.desc]
      )
    )
  })
  covar.info.long <- rbindlist(covar.info.long)
  
  # add proper sample name
  m1 <- match(covar.info.long$assay.sample, assay.info$assay)
  covar.info.long$assay.val <- assay.info[(m1), assay.val]
  
  # create a column for matching to the "abundance_assay[...]" id from the sml table
  covar.info.long[, metab.id := paste0("abundance_", assay.sample)]
  
  # Format SML data ----
  
  # id columns for features
  id.cols <- c("chemical_name", "database_identifier", "chemical_formula", "SML_ID", "SMF_ID_REFS", "unique_id")
  assay.cols <- grep(value = TRUE, pattern = "^abundance\\_assay\\[\\d+\\]$", x = names(sml))
  
  # create backup unique_id column
  sml[, unique_id := paste0("feature_", 1:.N)]
  
  # choose feature ID
  dupli.id <- sapply(id.cols, function(i){
    
    # check whether any ID is duplicated
    return(any(duplicated(sml[[i]])))
    
  })
  
  # select the best unique feature id column
  feature.id <- id.cols[!dupli.id][1]
  
  # metab data
  mzmetabdat <- sml[,.SD,.SDcols=c(feature.id, assay.cols)]
  
  # transpose data
  mzmetabdat <- dcast(
    melt(
      mzmetabdat, 
      id.vars = feature.id), 
    formula = as.formula(paste0("variable ~ ", feature.id)),
    value.var = "value")
  
  # add columns for ID and study_id and batch_id dummies
  mzmetabdat <- cbind(
    assay.info[match(mzmetabdat$variable, abundance.assay), .(assay, assay.val, sample, sample.val, run)],
    mzmetabdat 
  )

  # covariate data
  mzcovardat <- cbind(
    assay.info[match(covar.info.long$assay.sample, assay), .(assay, sample, sample.val, run)],
    covar.info.long
  )
  
  # Clean ----
  
  # change "null", NaN to NA
  # try to guess NA characters that might hide in the data
  mzmetabdat[, names(mzmetabdat) := lapply(.SD, function(i){
    i[i %in% c("", " ", "-", "/", "NA","null", "NULL", NULL, Inf, NaN)] <- NA
    i
  })]
  
  mzcovardat[, names(mzcovardat) := lapply(.SD, function(i){
    i[i %in% c("", " ", "-", "/", "NA","null", "NULL", NULL, Inf, NaN)] <- NA
    i
  })]
  
  # remove all NA cols
  # metab NA
  na.cols <- sapply(names(mzmetabdat),function(i){
    all(is.na(mzmetabdat[[i]]))
  })
  mzmetabdat[, names(na.cols[na.cols==T]) := NULL]
  
  # covar NA
  na.cols <- sapply(names(mzcovardat),function(i){
    all(is.na(mzcovardat[[i]]))
  })
  mzcovardat[, names(na.cols[na.cols==T]) := NULL]
  
  
  # add dummy columns for batch and study
  mzmetabdat[, study_dummy := "study_A"]
  
  if(is.null(mzmetabdat$run)){ 
    mzmetabdat[, study_dummy := "study_A"] 
  }

  
  
  # Finish ----
  
  return(list(
    metab = mzmetabdat,
    covar = mzcovardat
  ))
  
}