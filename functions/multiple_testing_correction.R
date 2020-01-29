multiple_testing_correction <- function(
  dataObjectSelected,
  dataObjectRemaining,
  correctionMethod
){
  # re-assign input
  res.selected <- dataObjectSelected
  res.remaining <- dataObjectRemaining
  multiple.testing.correction <- correctionMethod
  
  # merge data from all partial r.squared objects
  to.merge <- list(res.selected, 
                   res.remaining)
  all.multi <- rbindlist(
    to.merge,
    use.names = T, fill = TRUE)
  
  # remove NA elements from merging (in case no covariate was selected)
  all.multi <- all.multi[!is.na(cohort),]
  
  # return empty data in case there are missings in the pval column
  if(any(is.na(all.multi$p.value))){
    
    p.col <- "error"
    all.multi[,error:="Missings in p-value column. Please reconsider your filters!"]
    all.multi[,term.r.squared:=NA]
    message("Missings found in the p-value column. No multiple testing correction possible. Reconsider the set filters and re-run the analysis.")
    setnames(all.multi, old = c("term", "response"), new = c("covariate", "metabolite"))
    return(all.multi)
    
  } else if(multiple.testing.correction == "fdr"){
    
    p.col <- "p.fdr"
    all.multi[, p.fdr := p.adjust(p.value, method = "BH"), by =  .(cohort)]
    
  }else if(multiple.testing.correction == "bonferroni"){
    
    p.col <- "p.bonferroni"
    all.multi[, p.bonferroni := p.adjust(p.value, method = "bonferroni"), by =  .(cohort)]
    
  } else if(multiple.testing.correction == "hierarchical.bf"){
    
    # # Bonferroni
    # p.col <- "p.hierarchical.bonferroni"
    # all.multi[
    #   , p.hierarchical.bonferroni :=
    #     addHierarchFDR(
    #       pvalues = p.value,
    #       categs = as.character(term),
    #       fdrmethod_level1 = "bonferroni",
    #       fdrmethod_level2 = "bonferroni",
    #       correctionLevel1 = "listlookup"
    #     )$fdr_level1,
    #   by = .(cohort)]
    
    
    hier <- all.multi[, addHierarchFDR(
      pvalues = p.value,
      categs = as.character(term),
      fdrmethod_level1 = "bonferroni",
      fdrmethod_level2 = "bonferroni",
      correctionLevel1 = "listlookup"
    ),
    by = .(cohort)]
    
    p.col <- c("p.hierarchical.bonferroni.level1", "p.hierarchical.bonferroni.level2", "p.hierarchical.bonferroni.sig")
    all.multi$p.hierarchical.bonferroni.level1 <- hier$fdr_level1
    all.multi$p.hierarchical.bonferroni.level2 <- hier$fdr_level2
    all.multi$p.hierarchical.bonferroni.sig <- hier$hierarch_fdr5proz
    
    
  } else if(multiple.testing.correction == "hierarchical.bb"){
    
    # Benjamini Bogomolov -> standard BH-BH and BB second level adjustment
    # p.col <- "p.benjamini.bogomolov"
    # all.multi[
    #   , p.benjamini.bogomolov := 
    #     addHierarchFDR(
    #       pvalues = p.value,
    #       categs = as.character(term)
    #     )$fdr_level1, by = .(cohort)]
    
    hier <- all.multi[
      , p.benjamini.bogomolov := 
        addHierarchFDR(
          pvalues = p.value,
          categs = as.character(term)
        ), by = .(cohort)]
    
    p.col <- c("p.benjamini.bogomolov.level1", "p.benjamini.bogomolov.level2", "p.benjamini.bogomolov.sig")
    all.multi$p.benjamini.bogomolov.level1 <- hier$fdr_level1
    all.multi$p.benjamini.bogomolov.level2 <- hier$fdr_level2
    all.multi$p.benjamini.bogomolov.sig <- hier$hierarch_fdr5proz
    
    
    
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