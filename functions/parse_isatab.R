parse_isatab <- function(isaraw = NULL){
  
  type <- rawdat[1]
  
  if(type=="mwtab") {
    
    dat <- parse_mwtab()
    
  } else if(type=="mztab") {
    
    dat <- parse_mwtab()
    
  } else {
    
    dat <- ("No recognized format!")
    
  }
  
}