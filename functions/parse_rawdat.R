parse_rawdat <- function(rawdat = NULL){
  
  # check format type
  if(grepl(pattern = "MTD\tmzTab-version\t2.2.0-M",fixed = T, x = rawdat[1])){
    
    raw.type <- "mztab"
    
  } else if(grepl(pattern = "METABOLOMICS WORKBENCH",fixed = T, x = rawdat[1])){
    
    raw.type <- "mwtab"
    
  } else {
    
    raw.type <- "none"
    
  }
    
  # apply functions 
  if(raw.type=="mwtab") {
    
    dat <- parse_mwtab(rawdat)
    
  } else if(raw.type=="mztab") {
    
    dat <- parse_mztab(rawdat)
    
  } else if(raw.type=="none") {
    
    dat <- list(metab = data.table(c("No recognized format!",
                                     "If you uploaded an mwtab or mzTab-M file, the (experimental) parser did not recognize the file header!")),
                covar = data.table(c("No recognized format!",
                                     "If you uploaded an mwtab or mzTab-M file, the (experimental) parser did not recognize the file header!")))
    
  } else {
    
    stop("Something went horribly wrong!")
    
  }
  
  # remove singluar & NA columns
  if(raw.type %in% c("mwtab", "mztab")){
    
    na.cols <- sapply(names(dat$metab)[!(names(dat$metab) %in% c("dummy.study", "dummy.batch"))],function(i){
      all(is.na(dat$metab[[i]])) | (is.numeric(dat$metab[[i]]) & uniqueN(dat$metab[[i]]) == 1)
    })
        dat$metab[, names(na.cols[na.cols==T]) := NULL]
    
    na.cols <- sapply(names(dat$covar),function(i){
      all(is.na(dat$covar[[i]])) | (is.numeric(dat$covar[[i]]) & uniqueN(dat$covar[[i]]) == 1)
    })
    dat$covar[, names(na.cols[na.cols==T]) := NULL]
    
  }
  
  return(dat)
  
}