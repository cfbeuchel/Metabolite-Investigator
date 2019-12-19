# install dependencies when using github package
source("functions/install_dependencies.R") # REMOVE_IN_GITLAB_TAG
install_dependencies() # REMOVE_IN_GITLAB_TAG

# Important dependency for hosting on shinyapps.io -----------------------
options(repos = BiocManager::repositories())
getOption("repos")

# load required packages -> This doesn't work with parallel hosting on shinyapps.io -> breaks dependencies
# for (i in necessary.packages) {
#   suppressPackageStartupMessages(library(i, character.only = TRUE))
# }

# seperate calls to all packages? -> package dependency is otherwise not recognized by shinyapps.io
library("BiocManager")
library("data.table")
library("corrplot")
library("sva")
library("shiny")
library("ggplot2")
library("magrittr")
library("visNetwork")
library("scales")
library("lmtest")

# shiny Options - enable larger Upload sizes
options(shiny.maxRequestSize=30*1024^2) 

# Functions ---------------------------------------------------------------
source("functions/test_r2_distribution.R")
source("functions/custom_corrplot.R")
source("functions/format_for_custom_corrplot.R")
source("functions/interaction_partial_r_squared_multi.R")
source("functions/interaction_partial_r_squared_uni.R")
source("functions/make_matrices.R")
source("functions/network_plot.R")
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

# Thx @Jonas Wagner for this simple solution to the weird empty plot bug
pdf(NULL)
