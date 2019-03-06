multiple_testing_correction <- function(
  dataObjectSelected,
  dataObjectRemaining,
  correctionMethod
){
  # re-assign input
  res.selected <- dataObjectSelected
  res.remaining <- dataObjectRemaining
  multiple.testing.correction <- correctionMethod
  
  # remove NA elements from merging (in case no covariate was selected)
  to.merge <- list(res.selected, 
                   res.remaining)
  to.merge <- to.merge[!is.na(to.merge)]
  
  # merge data from all partial r.squared objects
  all.multi <- rbindlist(
    to.merge,
    use.names = T)
  
  if(multiple.testing.correction == "fdr"){
    
    p.col <- "p.fdr"
    all.multi[, p.fdr := p.adjust(p.value, method = "BH"), by =  .(cohort)]
    
  }else if(multiple.testing.correction == "bonferroni"){
    
    p.col <- "p.bonferroni"
    all.multi[, p.bonferroni := p.adjust(p.value, method = "bonferroni"), by =  .(cohort)]
    
  } else if(multiple.testing.correction == "hierarchical.bf"){
    
    # Bonferroni
    p.col <- "p.hierarchical.bonferroni"
    all.multi[
      , p.hierarchical.bonferroni :=
        addHierarchFDR(
          pvalues = p.value,
          categs = as.character(term),
          fdrmethod_level1 = "bonferroni",
          fdrmethod_level2 = "bonferroni",
          correctionLevel1 = "listlookup"
        )$fdr_level1,
      by = .(cohort)]
    
  } else if(multiple.testing.correction == "hierarchical.bb"){
    
    # Benjamini Bogomolov -> standard BH-BH and BB second level adjustment
    p.col <- "p.benjamini.bogomolov"
    all.multi[
      , p.benjamini.bogomolov := 
        addHierarchFDR(
          pvalues = p.value,
          categs = as.character(term)
        )$fdr_level1, by = .(cohort)]
    
  } else {
    stop("Non-avaliable multiple-testing correction method supplied. Please choose one of the four available methods.")
  }
  
  # reorder columns
  setnames(all.multi, old = c("term", "response"), new = c("covariate", "metabolite"))
  all.multi[, .SD, .SDcols = c("cohort",
                               "covariate",
                               "metabolite",
                               "estimate",
                               "std.error",
                               "term.r.squared",
                               "model.r.squared",
                               "p.value",
                               p.col,
                               "n",
                               "r.squared.cutoff"
                               )]
  
  return(all.multi)
}