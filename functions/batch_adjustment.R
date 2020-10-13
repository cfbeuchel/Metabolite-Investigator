batch_adjustment <- function(dat,
                             batch,
                             parameters,
                             batch.date = NA,
                             phenos = NA,
                             my.par.prior = F) {
  
  # designate transposed matrix for adjustment
  my.edata <- dat[, t(as.matrix(.SD)), .SDcols = parameters]
  
  # set batch as character string
  my.batch <- dat[, unname(unlist(.SD)), .SDcols = batch]
  
  # create formula in case phenos are not NA
  if(!all(is.na(phenos))) {
    
    # set phenotypes to adjust
    my.pheno <- dat[, .SD, .SDcols = c(batch, phenos)]
    
    # add covars
    my.formula <- as.formula(paste0("~ 1 + ", paste(phenos, collapse = " + ")))
    
  } else {
    
    # set phenotypes to adjust
    my.pheno <- dat[, .SD, .SDcols = batch]
    
    # only intercept term, no covariates
    my.formula <- ~ 1
  }
  
  # create model matrix
  my.modcombat <- model.matrix(my.formula, data = my.pheno)
  
  # run combat non-parametrically
  combat.edata <- ComBat(dat = my.edata,
                         batch = my.batch,
                         mod = my.modcombat,
                         par.prior = my.par.prior,
                         prior.plots = TRUE)
  
  # reset plot pars
  par(mfrow = c(1,1))
  
  # re-enter adjusted data into dat
  dat.dummy <- copy(dat)
  dat.dummy[, (parameters) := as.data.table(t(combat.edata))]
  
    return(dat.dummy)
}
