max_threads <- function(){
  
  as.integer(
    str_extract(
      extract = "\\d+",
      
      string = capture.output(getDTthreads(verbose=TRUE))[7])
  )
  
}