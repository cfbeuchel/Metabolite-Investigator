parse_mwtab <- function(mwraw = NULL) {
  
  ### function to process data following the mwTab format ###
  # https://www.metabolomicsworkbench.org/data/mwTab_specification.pdf
  
  # read metabolite data ----
  
  mwstart <- grep(pattern = "^MS_METABOLITE_DATA_START|^NMR_BINNED_DATA_START", mwraw)
  mwend <- grep(pattern = "^MS_METABOLITE_DATA_END|^NMR_BINNED_DATA_END", mwraw)
  mwmsdat <- mwraw[(mwstart):(mwend-1)]
  suppressWarnings(mwdat <- fread(text = mwmsdat))
  col.0 <- names(mwdat)
  names(mwdat) <- paste0("col_",1:ncol(mwdat))
  mwdat <- dcast(melt(mwdat,id.vars = 1),variable~col_1)
  mwdat$variable <- NULL
  mwdat$Factors <- NULL
  mwdat$sample.id <- if(is.null(mwdat$Samples)){
    col.0[-1]
  }else{
    mwdat$Samples
  }
  mwdat$Samples <- NULL
  
  # read covar data ----
  
  # covar dat
  mwcovars <- grep(pattern = "SUBJECT_SAMPLE_FACTORS", mwraw)[-1]
  mwcovardat <- fread(text=mwraw[mwcovars],sep="\t")
  names(mwcovardat) <- paste0("col_",1:ncol(mwcovardat))
  nacols <- mwcovardat[,lapply(.SD, function(x) all(is.na(x)))]
  mwcovardat <- mwcovardat[,.SD,.SDcols=names(mwcovardat)[!nacols]]
  names(mwcovardat) <- c("header","subject","sample.id","short.covars","long.covars")[1:ncol(mwcovardat)]
  mwcovardat$header <- NULL
  
  # split the third column ----
  
  # seperate each name/value pair thats separated by |
  
  # seperate only when | is in col
  if(all(grepl(pattern = "|", fixed = TRUE, x = mwcovardat$short.covars))){
    
    tmp1 <- strsplit(gsub(x = mwcovardat$short.covars,
                          pattern = " ",
                          replacement = "",
                          fixed = T),split = "|",fixed = T)
    
    # collect each pair in a seperate vector
    n.items <- length(tmp1[[1]])
    tmp2 <- lapply(1:n.items, function(i){
      sapply(tmp1,`[[`,i)
    })
    
  } else {
    
    # if no seperator is found: don't seperate
    tmp2 <- list(mwcovardat$short.covars)
    
  }
  
  if(all(grepl(pattern = ":", fixed = TRUE, x = tmp2))){
    
    # extract the name to later name the columns
    col.names <- sapply(tmp2,function(i){
      strsplit(i[1],split = ":",fixed = TRUE)[[1]][1]
    })
    
    # create a data.table and remove the name part from each pair
    setDT(tmp2)
    tmp2[, names(tmp2) := lapply(.SD,function(i){
      gsub(pattern = "^.*\\:", replacement = "",x = i)
    })]
    names(tmp2) <- col.names
    
  } else {
    
    setDT(tmp2)
    names(tmp2) <- paste0("col_",1:ncol(tmp2))
    
  }
  
  # check for duplicated column names and append a number
  if(any(duplicated(names(tmp2)))){
    dupl <- names(tmp2)[duplicated(names(tmp2))]
    for(i in dupl){
      old <- names(tmp2)[names(tmp2) %in% i]
      names(tmp2)[names(tmp2) %in% i] <- paste0(old, "_", 1:length(old))
    }
  }
  
  # enter into data
  mwcovardat$short.covars <- NULL
  mwcovardat[, names(tmp2) := tmp2]
  
  # split the fourth column ----
  
  # check if column is present
  if(!is.null(mwcovardat$long.covars)){
    
    if(all(grepl(pattern = ";", fixed = TRUE, x = mwcovardat$long.covars))){
      
      tmp1 <- strsplit(gsub(x = mwcovardat$long.covars,
                            pattern = " ",
                            replacement = "",
                            fixed = T),split = ";",fixed = T)
      
      # collect each pair in a seperate vector
      n.items <- length(tmp1[[1]])
      tmp2 <- lapply(1:n.items, function(i){
        sapply(tmp1,`[[`,i)
      })
      
    } else {
      
      # if no seperator is found: don't seperate
      tmp2 <- list(mwcovardat$long.covars)
      
    }
    
    if(all(grepl(pattern = "=", fixed = TRUE, x = tmp2))) {
      
      # extract the name to later name the columns
      col.names <- sapply(tmp2,function(i){
        strsplit(i[1],split = "=",fixed = TRUE)[[1]][1]
      })
      
      # create a data.table and remove the name part from each pair
      setDT(tmp2)
      tmp2[, names(tmp2) := lapply(.SD,function(i){
        gsub(pattern = "^.*\\=", replacement = "",x = i)
      })]
      names(tmp2) <- col.names
      
    } else {
      
      setDT(tmp2)
      names(tmp2) <- paste0("col_",1:ncol(tmp2))
      
    }
    
    # enter into data
    mwcovardat$long.covars <- NULL
    mwcovardat[, names(tmp2) := tmp2]
    
  }
  
  # clean ----
  
  # COVAR DATA
  
  # try to guess NA characters that might hide in the data
  mwcovardat[, names(mwcovardat) := lapply(.SD, function(i){
    i[i %in% c("", " ", "-", "/", "NA", NULL, Inf)] <- NA
    i
  })]
  
  # remove all NA cols
  na.cols <- sapply(names(mwcovardat),function(i){
    all(is.na(mwcovardat[[i]]))
  })
  mwcovardat[, names(na.cols[na.cols==T]) := NULL]
  
  
  # find character containing cols 
  num.cols <- sapply(names(mwcovardat), function(i){
    !any(grepl("[a-zA-Z]",mwcovardat[[i]]))
  })
  num.cols <- names(num.cols)[num.cols==T]
  
  # do not change the sample id in case it is numeric
  num.cols <- num.cols[num.cols != "sample.id"]
  
  # try to set numeric columns to correct type
  mwcovardat[, (num.cols) := lapply(.SD, function(i){
    as.numeric(i)
  }),.SDcols=num.cols]
  
  # METAB DATA
  
  # try to guess NA characters that might hide in the data
  mwdat[, names(mwdat) := lapply(.SD, function(i){
    i[i %in% c("", " ", "-", "/", "NA", NULL, Inf)] <- NA
    i
  })]
  
  # remove all NA cols
  na.cols <- sapply(names(mwdat),function(i){
    all(is.na(mwdat[[i]]))
  })
  mwdat[, names(na.cols[na.cols==T]) := NULL]
  
  # set everything numeric
  # try to set numeric columns to correct type
  metab.cols <- names(mwdat)[names(mwdat) != "sample.id"]
  mwdat[, (metab.cols) := lapply(.SD, function(i){
    as.numeric(i)
  }),.SDcols=metab.cols]
  
  # create dummy columns for batch and study
  mwdat[, dummy.batch := "dummy_batch"]
  mwdat[, dummy.study := "dummy_study"]
  
  # finish ----
  
  setcolorder(mwdat, c(
    "dummy.study",
    "dummy.batch", 
    "sample.id", 
    names(mwdat)[!(names(mwdat) %in% c("sample.id", "dummy.batch", "dummy.study"))]))
  
  return(list(
    metab = mwdat,
    covar = mwcovardat
  ))
  
}
