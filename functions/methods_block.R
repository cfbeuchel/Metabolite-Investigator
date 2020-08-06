methods_block <- function(
  outlier.check,
  max.removed, 
  int.check,
  batch.check,
  batch.name,
  n.batches,
  study.names,
  non.combat.metabs,
  non.combat.studies,
  corr.cutoff,
  r.sqr.min,
  pval.corr
  
){
  
  part.intro <- "Metabolites were pre-processed prior to analysis. In order to stabilize regression analysis, "
  
  if(outlier.check == TRUE) {
    
    part.outlier <- paste0(
      "outliers were removed cohort-wise by applying a cutoff of mean + 5 × SD of the logarithmized data. Zero values were excluded for this purpose. In our hands, outlier analysis removed a maximum of ", 
      max.removed, 
      " measurements per metabolite and cohort. "
      )
    
  } else {
    
    part.outlier <- NULL  
    
  }
  
  if(int.check == TRUE) {
    
    part.int <- "Remaining metabolite data were inverse-normal-transformed. "
    
  } else {
    
    part.int <- NULL
    
  }
  
  if(batch.check == TRUE) {
    
    part.batch <- paste0(
      "Effects of known technical batches (e.g. analysis plate ID) are removed by a non-parametric empirical method as implemented in function ‘ComBat’ [https://doi.org/10.1093/biostatistics/kxj037] of the R-package ‘sva’ [https://doi.org/10.1093/bioinformatics/bts034]. We considered ",
      batch.name,
      " as batch variable, resulting in ",
      n.batches,
      " batches for ", 
      study.names, 
      ", respectively. Since the ‘ComBat’ procedure requires complete data, missing values were mean-imputed, using within-batch data or all data when a certain metabolite was completely missing in a batch. After batch correction, imputed data points were set missing again. For ", 
      non.combat.metabs, 
      " in ", 
      non.combat.studies, 
      ", batch affects were removed by residualization via a linear mixed model due to small batch variance. "
    )
    
  } else {
    
    part.batch <- NULL
    
  }
  
  # correlation cutoff ----
  
  part.corr <- paste0(
    "Prior to association analysis with metabolites, all continuous clinical or lifestyle parameters were mean-centered and scaled to one standard deviation (SD). For association analysis, metabolites were univariately associated with the clinical/lifestyle parameters by linear regression analysis. For multivariable analysis, correlated factors were pruned to avoid collinearity and to improve interpretation (default Pearson's |r| > ", 
    corr.cutoff, 
    " in any cohort [https://doi.org/10.4172/2161-1165.1000227]). In case of correlated factors, we preferred those which are clinically more often evaluated. "
  )
  
  # factor selection ----
  
  part.selection <- paste0(
    "Factors with a relevant level of association to the metabolites were selection by performing a backward selection based on the variance of the metabolites explained by every factor, seperately for each study. Factors explaining more than ", 
    r.sqr.min, 
    " of the variance of at least one metabolite in at least one study were considered relevant. For this, factors not explaining the variance threshold were removed iteratively in each study until all factors met the selection criteria. The union of the factors across all studies were considered relevant when each met the variance threshold in at least one metabolite in one study when fit jointly as covariates. "
  )
  
  # multiple testing ----
  
  part.pval.corr.start <- "To account for multiple testing of all metabolites and factors, we applied "
  
  if (pval.corr == "hierarchical.bf") {
    
    # bonferroni hierarchical
    part.pval.corr <- "a Bonferroni p-value correction [https://doi.org/10.1038/nbt1209-1135] in a hierarchical way, considering each tested factor as a family of hypotheses regarding metabolite association ([https://doi.org/10.1002/gepi.21942], [https://doi.org/10.1093/nar/gky780]). "
    
  } else if (pval.corr == "hierarchical.bb") {
    
    # fdr hierarchical
    part.pval.corr <- "a p-value correction to an FDR=5% [https://doi.org/10.1111/j.2517-6161.1995.tb02031.x] in a hierarchical way, considering each tested factor as a family of hypotheses regarding metabolite association ([https://doi.org/10.1002/gepi.21942], [https://doi.org/10.1093/nar/gky780]). "
    
  } else if (pval.corr == "bonferroni") {
    
    part.pval.corr <- "a Bonferroni p-value correction [https://doi.org/10.1038/nbt1209-1135]. "
    
  } else if(pval.corr == "fdr"){
    
    part.pval.corr <- "a p-value correction to an FDR=5% [https://doi.org/10.1111/j.2517-6161.1995.tb02031.x]. "
    
  }
  
  part.r2.descr <- "Effect sizes of metabolite associations are assessed by the explained variance (r2) of the considered factor in a univariable model or as partially explained variance in a multivariable regression model. "
  
  # difference in r2 distribution ----
  
  part.distribution.comp <- "For every factor, we quantify the difference in the distribution of r2 between cohorts by Friedman test followed by Benjamini-Hochberg correction for multiple testing. When two distributions were compared, the Wilcoxon signed rank test was used. "
  
  # network plots ----
  
  part.networks <- " Bi-partite networks, connecting metabolite nodes, and factor nodes with edges representing the partial explained variance were created using ‘visNetwork’ [https://CRAN.R-project.org/package=visNetwork]."
  
  # combine ----
  
  text <- paste0(
    part.intro,
    part.outlier,
    part.int,
    part.batch,
    part.corr,
    part.selection,
    part.pval.corr.start,
    part.pval.corr,
    part.r2.descr,
    part.distribution.comp,
    part.networks
    
  )
  
  return(text)
  
}
