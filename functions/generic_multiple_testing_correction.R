generic_multiple_testing_correction <- function(
  data,
  correctionMethod
){
  # re-assign input
  multiple.testing.correction <- correctionMethod

  
  if(multiple.testing.correction == "fdr"){
    
    p.col <- "p.fdr"
    data[, p.fdr := p.adjust(p.value, method = "BH"), by =  .(cohort)]
    
  }else if(multiple.testing.correction == "bonferroni"){
    
    p.col <- "p.bonferroni"
    data[, p.bonferroni := p.adjust(p.value, method = "bonferroni"), by =  .(cohort)]
    
  } else if(multiple.testing.correction == "hierarchical.bf"){
    
    # Bonferroni
    p.col <- "p.hierarchical.bonferroni"
    data[
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
    data[
      , p.benjamini.bogomolov := 
        addHierarchFDR(
          pvalues = p.value,
          categs = as.character(term)
        )$fdr_level1, by = .(cohort)]
    
  } else {
    stop("Non-avaliable multiple-testing correction method supplied. Please choose one of the four available methods.")
  }

  return(data)
}