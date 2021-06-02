generic_multiple_testing_correction <- function(
  data,
  correctionMethod
){
  # re-assign input
  multiple.testing.correction <- correctionMethod
  
  # remove NA p-value columns 
  data <- data[!is.na(p.value)]
  
  if(multiple.testing.correction == "fdr"){
    
    p.col <- "p.fdr"
    data[, p.fdr := p.adjust(p.value, method = "BH"), by =  .(cohort)]
    data$tmp.sig <- data$p.fdr<=0.05
    data$tmp.p.adj <- data$p.fdr
    
    
  }else if(multiple.testing.correction == "bonferroni"){
    
    p.col <- "p.bonferroni"
    data[, p.bonferroni := p.adjust(p.value, method = "bonferroni"), by =  .(cohort)]
    data$tmp.sig <- data$p.fdr<=0.05
    data$tmp.p.adj <- data$p.bonferroni
    
  } else if(multiple.testing.correction == "hierarchical.bf"){
    
    # Bonferroni
    p.col <- "p.hierarchical.bonferroni"
    hier <- data[, c("p","category", "fdr_level1", "fdr_level2", "hierarch_fdr5proz") := addHierarchFDR(
        pvalues = p.value,
        categs = as.character(term),
        fdrmethod_level1 = "bonferroni",
        fdrmethod_level2 = "bonferroni",
        correctionLevel1 = "listlookup"
      ),
      by = .(cohort)]
    
    data$p.hierarchical.bonferroni.level1 <- hier$fdr_level1
    data$p.hierarchical.bonferroni.level2 <- hier$fdr_level2
    data$p.hierarchical.bonferroni.sig <- hier$hierarch_fdr5proz
    data$tmp.sig <- data$p.hierarchical.bonferroni.sig
    data$tmp.p.adj <- data$p.hierarchical.bonferroni.level1
    data[,c("p","category", "fdr_level1", "fdr_level2", "hierarch_fdr5proz") := NULL]
    
    } else if(multiple.testing.correction == "hierarchical.bb"){
    
    # Benjamini Bogomolov -> standard BH-BH and BB second level adjustment
    p.col <- "p.benjamini.bogomolov"
    hier <- data[, c("p","category", "fdr_level1", "fdr_level2", "hierarch_fdr5proz") := addHierarchFDR(
      pvalues = p.value,
      categs = as.character(term),
      fdrmethod_level1 = "BH",
      fdrmethod_level2 = "BH",
      correctionLevel1 = "BB"
    ),
    by = .(cohort)]
    
    data$p.benjamini.bogomolov.level1 <- hier$fdr_level1
    data$p.benjamini.bogomolov.level2 <- hier$fdr_level2
    data$p.benjamini.bogomolov.sig <- hier$hierarch_fdr5proz
    data$tmp.sig <- data$p.benjamini.bogomolov.sig
    data$tmp.p.adj <- data$p.benjamini.bogomolov.level1
    data[,c("p","category", "fdr_level1", "fdr_level2", "hierarch_fdr5proz") := NULL]
    
  } else {
    stop("Non-avaliable multiple-testing correction method supplied. Please choose one of the four available methods.")
  }

  return(data)
}
