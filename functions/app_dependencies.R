# check for installed packages and install them if necessary
installed <- installed.packages()
needed <- c("data.table","sva","shiny","ggplot2","scales")
to.install <- needed[!(needed %in% installed[,1])]
if(length(to.install)!=0){
  if("sva" %in% to.install){
    install.packages("BiocManager")
    BiocManager::install("sva")
  }
  install.packages(to.install[to.install!="sva"], repos = "https://cran.uni-muenster.de/")
}

# load required packages
for (i in c(
  "data.table",
  "sva",
  "shiny",
  "ggplot2",
  "scales"
)) {
  suppressPackageStartupMessages(library(i, character.only = TRUE))
}

# shiny Options - enable larger Upload sizes
options(shiny.maxRequestSize=30*1024^2) 

# Functions ---------------------------------------------------------------
source("functions/plot_correlation.R")
source("functions/multivariable_assoc.R")
source("functions/univariable_assoc.R")
source("functions/plot_multivar.R")
source("functions/plot_univar.R")
source("functions/change_in_dt.R")
source("functions/data_merging.R")
source("functions/pre_process_metabolites.R")
source("functions/sd_filter_vector.R")
source("functions/data_reduction_by_filtering.R")
source("functions/inverse_normal_transform.R")
source("functions/check_batch_effects.R")
source("functions/impute_missings_for_combat.R")
source("functions/batch_adjustment.R")
source("functions/restore_missings_after_combat.R")
source("functions/show_highly_correlating.R")
source("functions/find_relevant_covariates.R")
source("functions/partial_r_squared.R")
source("functions/relevance_backwards_selection.R")
source("functions/add_hierarchical_FDR.R")
source("functions/multiple_testing_correction.R")
source("functions/generic_multiple_testing_correction.R")
source("functions/remaining_covariates.R")
source("functions/partial_r_squared_excluded_covars.R")
