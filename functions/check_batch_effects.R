check_batch_effects <- function(Metabolite, 
                                Batch){
  
  # calculate a non-parametric test for group differences between batches
  test.dat <- data.table(metabolite = Metabolite,
                         batch      = Batch)
  
  k.w.mod <- kruskal.test(
    formula = as.formula("metabolite ~ factor(batch)"), data = test.dat)
  
  # return p-value
  return(k.w.mod$p.value)
}