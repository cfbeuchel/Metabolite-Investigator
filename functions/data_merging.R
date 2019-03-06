data_merging <- function(
  covariateData,
  covariateID,
  covariateColumns,
  metaboliteData,
  metaboliteID,
  metaboliteColumns,
  cohortID,
  batchID) {
  
  # tests #TODO#
  # if(any(sapply(c(covariateData,
  #                covariateID,
  #                covariateColumns,
  #                metaboliteData,
  #                metaboliteID,
  #                metaboliteColumns,
  #                cohortID,
  #                batchID), missing))){
  #   stop("Not all necessary variables defined!")
  # }
  
  # define covariate & metabolite ID column to merge by
  covar.id <- covariateID #USER_INPUT#
  metab.id <- metaboliteID #USER_INPUT#
  input.covar <- covariateData #USER_INPUT#
  input.metab <- metaboliteData #USER_INPUT#
  cohort.col <- cohortID #USER_INPUT#
  batch.col <- batchID #USER_INPUT#
  m.cols <- metaboliteColumns #USER_INPUT#
  # c("Gln", "Lys", "OHProl", "PiPA", "Aba", "Ala", "Arg", "Asn", 
  #           "Asp", "Carnosin", "Cit", "Glu", "Gly", "His", "LeuIle", "MeHis", 
  #           "Met", "Orn", "Phe", "Pro", "Sarc", "Ser", "Tau", "Thr", "Trp", 
  #           "Tyr", "Val", "C0", "C2", "C3", "C3DC", "C4", "C4OH", "C5", "C5OHHMG", 
  #           "C51", "C6", "C6DC", "C8", "C81", "C10", "C101", "C12", "C14", 
  #           "C141", "C14OH", "C16", "C161", "C161OH", "C16OH", "Glut", "MeGlut", 
  #           "MMA", "C18", "C181", "C181OH", "C182", "C182OH", "C18OH", "C201", 
  #           "C202", "C203", "acges")
  c.cols <- covariateColumns #USER_INPUT#
  # c("age", "sex", "log.bmi", "diabetes.status.tri", "diabetes.anamnese", 
  #           "diabetes.medication", "hba1c.percent", "smoking.status", "whr", 
  #           "bp.sys", "bp.dia", "pulse.pressure", "atc.code.g03", "cholesterol", 
  #           "ldl.cholesterol", "hdl.cholesterol", "white.blood.cells", "lymphocytes.percent", 
  #           "monocytes.percent", "hematocrit", "platelets", "reticulocytes", 
  #           "neutrophils.percent", "erythrocytes", "basophils.percent", "eosinophils.percent", 
  #           "blood.hemoglobin.level", "atc.code.c10")
  
  # turn into DT in case it isn't one
  setDT(input.covar)
  setDT(input.metab)
  
  # create new unified ID column
  setnames(input.covar, old = covar.id, new = "id", skip_absent = T)
  setnames(input.metab, old = metab.id, new = "id", skip_absent = T)
  
  # merge my seperate data into test data
  id.overlap <- intersect(input.covar$id, input.metab$id)
  message(paste0("Matching unique ID overlap for ", uniqueN(id.overlap), " samples found in metabolite and covariate data."))
  m.ordered <- match(id.overlap, input.metab$id)
  c.ordered <- match(id.overlap, input.covar$id)
  
  # merge into single data object
  dat <- cbind(input.covar[c.ordered, ], 
               input.metab[m.ordered, ])
  
  # remove duplicate id colum
  dat <- dat[, .SD, .SDcols = unique(names(dat))]
  
  
  # user specified columns for id, cohort and batch
  old.names <- c(cohort.col,
                 batch.col)
  
  # get a copy of the given data
  setnames(x   = dat,
           old = old.names,
           new = c("cohort",
                   "batch"), skip_absent = T)
  
  # only carry around necessary data
  dat <- dat[, .SD, .SDcols = c("cohort", "batch", "id", c.cols, m.cols)]
  
  # Which covariates are continuous and which categorical
  # categocical based on number of unique values
  c.unique <- dat[, sapply(.SD, function(x){
    y <- length(unique(x))
    y <= 5
  }), .SDcols = c.cols]
  
  # define categorical and continuous covariates
  c.categorical <- c.unique
  c.continuous <- !c.categorical
  
  # set column type
  # factor for categorical variables
  # numeric for continuous variables
  dat[, (c.cols[c.categorical]) := lapply(.SD, as.factor), .SDcols = c.cols[c.categorical]]
  dat[, (c.cols[c.continuous])  := lapply(.SD, as.numeric), .SDcols = c.cols[c.continuous]]
  
  # return results
  return(dat)
}
