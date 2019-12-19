install_dependencies <- function(){
  # which packages do I need for this app
  necessary.packages <- c(
    "BiocManager",
    "data.table",
    "corrplot",
    "sva",
    "shiny",
    "ggplot2",
    "magrittr",
    "visNetwork",
    "lmtest",
    "scales")
  
  # check for installed packages and install them if necessary
  installed <- installed.packages()
  needed <- necessary.packages
  to.install <- needed[!(needed %in% installed[,1])]
  rm(installed)
  if(length(to.install)!=0){
    if("sva" %in% to.install){
      install.packages("BiocManager")


      BiocManager::install("sva")
    }
    install.packages(to.install[to.install!="sva"], repos = "https://cran.uni-muenster.de/")
  }
  
}