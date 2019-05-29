# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # load dependencies
  source(file = "functions/app_dependencies.R")
  
  # for storage
  values <- reactiveValues()
  
  # show example data
  output$preview.example <- renderDataTable({
    data.table(id = paste0("Sample_", 1:12),
               cohort = paste0("cohort_", rep(c("a", "b"), each = 6)),
               batch = paste0("batch_", rep(1:3, each = 2)),
               metabolite.1 = rnorm(12),
               metabolite.2 = rnorm(12),
               metabolite.3 = rnorm(12))
  }, options = list(pageLength = 10))
  
  # Example Data ------------------------------------------------------------
  observeEvent(input$use.example, {
    
    # check for files
    validate(
      need(expr = file.exists("data/190307_simu_metabs.csv") & 
             file.exists("data/190307_simu_factors.csv"),
           message = "No example data found! Will be uploaded soon.")
    )
    
    # metab 
    values$input.metab <- (fread("data/190307_simu_metabs.csv"))
    output$preview.metab <- renderDataTable({values$input.metab}, options = list(pageLength = 10))
    
    # covar
    values$input.covar <- (fread("data/190307_simu_factors.csv"))
    output$preview.covar <- renderDataTable({values$input.covar}, options = list(pageLength = 10))
    
    # Mergin buttons
    # Update select input immediately after clicking on the action button.
    updateSelectInput(session, "metab.id","Select Metabolite ID Column", choices = names(values$input.metab), selected = names(values$input.metab)[3])
    updateSelectInput(session, "cohort.col","Select Metabolite Cohort ID Column", choices = names(values$input.metab), selected = names(values$input.metab)[1])
    updateSelectInput(session, "batch.col","Select Metabolite Batch ID Column", choices = names(values$input.metab), selected = names(values$input.metab)[2])
    updateSelectInput(session, "covar.id","Select Covariate ID Column", choices = names(values$input.covar))
    
    # output some text when using example data
    output$preview.text <- renderText({"Using example data (2 cohorts with each 5000 samples, 7 metabolites and 6 factors) for analysis."})
  })
  
  # preview covar data
  output$preview.covar <- renderDataTable({
    req(input$input.covar)
    tryCatch(
      {
        input.covar <- fread(input$input.covar$datapath,
                             sep = input$sep.covar,
                             quote = input$quote.covar)
          input.covar
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
  }, options = list(pageLength = 10))
  
  # Preview metabolite Data
  output$preview.metab <- renderDataTable({
    req(input$input.metab)
    tryCatch(
      {
        input.metab <- fread(input$input.metab$datapath,
                             sep = input$sep.metab,
                             quote = input$quote.metab)
          input.metab
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
  }, options = list(pageLength = 10))
  

  # Upload Metabolites ------------------------------------------------------
  observeEvent({
    input$input.metab
  },{
    # upload data
    values$input.metab <- fread(input$input.metab$datapath, sep=input$sep.metab)
    
    # Update select input immediately after clicking on the action button.
    updateSelectInput(session, "metab.id","Select Metabolite ID Column", choices = names(values$input.metab), selected = names(values$input.metab)[3])
    updateSelectInput(session, "cohort.col","Select Metabolite Cohort ID Column", choices = names(values$input.metab), selected = names(values$input.metab)[1])
    updateSelectInput(session, "batch.col","Select Metabolite Batch ID Column", choices = names(values$input.metab), selected = names(values$input.metab)[2])
    
    # output message
    output$metab.upload.success <- renderText("Metabolite data uploaded successfully. Please make sure it is the right data.")
  })
  

  # Upload Covariates -------------------------------------------------------
  observeEvent({
    input$input.covar
  },{
    # upload covar data
    values$input.covar <- (fread(input$input.covar$datapath, sep=input$sep.covar))
    
    # update button
    updateSelectInput(session, "covar.id","Select Covariate ID Column", choices = names(values$input.covar))
    
    # output message
    output$covar.upload.success <- renderText("Covariate data uploaded successfully. Please make sure it is the right data.")
  })
  
  # Covariate/Metabolite column selection -----------------------------------
  observeEvent(input$rest.covar,{
    if(input$rest.covar==F){
      updateSelectInput(session,
                        "covar.col",
                        "Select All Covariate Columns",
                        choices = names(values$input.covar)) # no choices before uploading
    }
  })
  
  observeEvent(input$rest.metab,{
    if(input$rest.metab==F){
      updateSelectInput(session,
                        "metab.col",
                        "Select All Metabolite Columns",
                        choices = names(values$input.metab)) # no choices before uploading
    }
  })
  
  
  # Data Merging -----------------------------------
  # rename metab/covar columns
  observeEvent(input$merge.button,{
    
    # assign values needed for merging
    values$covar.id <- input$covar.id
    values$metab.id <- input$metab.id
    values$cohort.col <- input$cohort.col
    values$batch.col <- input$batch.col
    
    # assign all remaining columns as metabolites/covariates
    if(input$rest.metab==T){
      values$m.cols <- setdiff(names(values$input.metab),
                               c(values$metab.id,
                                 values$cohort.col,
                                 values$batch.col))
    } else {
      values$m.cols <- input$metab.col
    }
    if(input$rest.covar==T){
      values$c.cols <- setdiff(names(values$input.covar),
                               c(values$covar.id))
    } else {
      values$c.cols <- input$covar.col
    }
    
    # valid selection of columns
    output$data.merge <- renderDataTable({
    validate(
      need(any(duplicated(c(values$metab.id,
                           values$cohort.col,
                            values$batch.col,
                            values$m.cols))) != T, "Batch and Cohort ID cannot be identical. Please review your selection.")
    )
    }, options = list(pageLength = 10))
    
    # Merging -----------------------------------------------------------------
    results <- data_merging(covariateData = values$input.covar,
                            metaboliteData = values$input.metab,
                            covariateID = values$covar.id,
                            metaboliteID = values$metab.id,
                            covariateColumns = values$c.cols,
                            metaboliteColumns = values$m.cols,
                            cohortID = values$cohort.col,
                            batchID = values$batch.col)
    
    # re-assign
    values$dat <- results[["dat"]]
    output$text.found.overlap <- renderText(results[["message"]])
    
    # helper text
    output$text.merge.upper <- renderText("Check if ID, batch and cohort columns have been labeled correctly. If everything seems in order, move to the next step.")

    # preview
    merged.preview <- values$dat[, .SD, .SDcols = c("cohort", "batch", "id", values$c.cols, values$m.cols)]
    output$data.merge <- renderDataTable({merged.preview}, options = list(pageLength = 10))
  })
  
  # Data preprocessing ------------------------------------------------------
  observeEvent(input$button.prepro, {
    
    dat <- isolate(values$dat)
    m.cols <- isolate(values$m.cols)
    c.cols <- isolate(values$c.cols)
    message("Starting pre-pro...")
    
    # start with metab annotation -------------------------
    missings.abs.m <- dat[, sapply(.SD, function(x){
      x <- is.na(x)
      sum(x)
    }), .SDcols = m.cols, by = cohort]
    
    missings.rel.m <- dat[, sapply(.SD, function(x){
      x <- is.na(x)
      sum(x)/.N
    }), .SDcols = m.cols, by = cohort]
    
    # calc zeros percentage in data as absolutes
    zero.infl.abs.m <- dat[, sapply(.SD, function(x){
      sum(x <= 0, na.rm = T)
    }), .SDcols = m.cols, by = cohort]
    
    # as relative values
    zero.infl.rel.m  <- dat[, sapply(.SD, function(x){
      sum(x <= 0, na.rm = T)/.N
    }), .SDcols = m.cols, by = cohort]
    
    # create an annotation table
    annot.m <- data.table(cohort = missings.abs.m$cohort,
                          metabolite = m.cols,
                          missings.absolute = missings.abs.m$V1,
                          missings.relative = missings.rel.m$V1,
                          zero.inflation.absolute = zero.infl.abs.m$V1,
                          zero.infaltion.relative = zero.infl.rel.m$V1)
    
    #================================================================
    
    # decide on prepro yes/no
    if(input$button.choose.prepro==T){
      pre.process.metabolites <- T 
    }else {
      pre.process.metabolites <- F
    }
    
    # preprocess data
    # single function pre-processing
    res <- pre_process_metabolites(
      preProcessMetabolites = pre.process.metabolites,
      preProcessingSteps = input$button.prepro.steps, # new
      dataObject = dat,
      metaboliteAnnotation = annot.m,
      metaboliteColumns = m.cols,
      covariateColumns = c.cols)
    
    # re-assign output from prepro function
    annot.m <- res$annot.m
    dat <- res$dat
    
    # covar annotation ------------------
    # calculate missings
    missings.abs.c <- dat[, sapply(.SD, function(x){
      x <- is.na(x)
      sum(x)
    }), .SDcols = c.cols, by = cohort]
    
    # relative missings
    missings.rel.c <- dat[, sapply(.SD, function(x){
      x <- is.na(x)
      sum(x)/.N
    }), .SDcols = c.cols, by = cohort]
    
    # create the annotation table
    annot.c <- data.table(cohort = missings.abs.c$cohort,
                          covariate = c.cols,
                          missings.absolute = missings.abs.c$V1,
                          missings.relative = missings.rel.c$V1
    )
    
    # define covariate type (continuous/discrete)
    # get column type
    c.unique <- dat[, sapply(.SD, is.factor), .SDcols = c.cols]
    c.continuous <- !c.unique
    annot.c[ ,covariate.type := ifelse(covariate %in% c.cols[c.continuous], "continuous", "discrete")]
    
    # display some of the data
    output$prepro.success <- renderText("Pre-processing of data was successful!")
    
    # Preview --------------------------
    # if(input$button.preview.prepro==T){
      # show annotation table
      output$prepro.annot.m <- renderDataTable({
        annot.m
      }, options = list(pageLength = 10))
      # show annotation table
      output$prepro.annot.c <- renderDataTable({
        annot.c
      }, options = list(pageLength = 10))
      
      # preview data
      output$prepro.data <- renderDataTable({
        dat
      }, options = list(pageLength = 10))
    # }
    
    # update selector for correlation
    updateSelectInput(session, "plot.corr.select","Display Correlation in Cohort", choices = unique(dat$cohort))
    updateSelectInput(session, "network.uni.select","Display bi-partite Network for", choices = unique(dat$cohort))
    updateSelectInput(session, "network.multi.select","Display bi-partite Network for", choices = unique(dat$cohort))
    
    # re-enter values into reactive
    values$annot.m <- annot.m
    values$annot.c <- annot.c
    values$dat <- dat
    values$m.cols <- m.cols
    values$c.cols <- c.cols
    values$c.cols.original <- c.cols
    
    prepro.description <- NULL
    
    output$prepro.description <- renderText({
      validate(
        need(!is.null(prepro.description), message = "This is not yet available. See future Updates for a detailed description.")
      )
    })
  }) # end of prepro
  
  # Univariable Association -------------------------------------------------
  observeEvent(input$univar.assoc.button, {
    
    message("Starting")
    
    # input
    dat <- isolate(values$dat)
    annot.c <- isolate(values$annot.c)
    m.cols <- isolate(values$m.cols)
    c.cols <- isolate(values$c.cols)
    multiple.testing <- isolate(input$univar.multiple.testing.correction.selecter)
    
    # compute univariable association results
    res.univar <- univariable_assoc(dometab = m.cols,
                                    docovar = c.cols,
                                    data = dat)
    
    res.univar <- generic_multiple_testing_correction(
      data = res.univar,
      correctionMethod = multiple.testing)
    
    # friedman/wilcoxon test for r2 difference
    r2.test <- test_r2_distribution(dat = res.univar,
                         r2Col = "r.squared")
    
    # add friedman/wilcoxon results to factor annotation
    r2.test.matched <- match(annot.c$covariate, r2.test$term)
    r2.test.join <- r2.test[(r2.test.matched), .SD, .SDcols = names(r2.test)[-1]]
    annot.c[, paste0("univar.", names(r2.test.join)) := r2.test.join]
    
    # annotate significance
    p.col <- tail(names(res.univar), 1)
    sig.factors <- res.univar[, .(sig = base::min(.SD) < 0.05,
                                  min.p = base::min(.SD)) ,by = .(term, cohort), .SDcols = p.col]
    annot.c[, tmp:=paste0(cohort, "_", covariate)]
    sig.factors[, tmp:=paste0(cohort, "_", term)]
    annot.c[match(sig.factors$tmp, tmp), univariable.min.p := sig.factors$min.p]
    annot.c[match(sig.factors$tmp, tmp), univariable.significant.p.0.05 := sig.factors$sig]
    annot.c$tmp <- NULL
    
    # built univariable network
    output$network.univar <- renderVisNetwork({
      
      # construct network plot
      network_plot(assocResults = copy(res.univar),
                   rSquaredColumn = "r.squared",
                   pColumn = p.col,
                   cohort = input$network.uni.select,
                   hierarchicalNetwork = input$network.uni.hierarch)})
    
    # plot the unviaraiable results
    output$plot.univar <- renderPlot({plot_univar(data = res.univar, rSquaredCol = "r.squared")})
    
    # reformat results for heatmap
    uni.max.matrix <- make_matrices(st2 = res.univar,
                                    r2Col = "r.squared",
                                    pCol = "p.hierarchical.bonferroni")
    
    # build heatmap of results
    output$heat.univar <- renderPlot({
      custom_corrplot(uni.max.matrix$r2matrix,
                      method="color",
                      is.corr = F,
                      p.mat = uni.max.matrix$pvalmatrix,
                      insig = "label_sig", 
                      sig.level=0.05, 
                      pch.col = rgb(0,0,0,0.66), 
                      col = colorRampPalette(c("blue", "grey95", "red"))(20),
                      title = "Maximimum (signed) R2 across all cohorts",
                      mar=c(0, 0, 4, 0),
                      cl.length = 11
      )
    }) 
    
    # save results for output
    output$res.univar <- renderDataTable(res.univar, options = list(pageLength = 10))
    values$res.univar <- res.univar
    values$annot.c <- annot.c
    
    # placeholder
    univar.description <- NULL
    
    # output methods text
    output$univar.description <- renderText({
      validate(
        need(!is.null(univar.description), message = "This is not yet available. See future Updates for a detailed description.")
      )
    })
    
    # return success message
    output$uni.success.text <- renderText({
      "Univariable associations calculated successfully. See the Plot-tab for a visualization and the Results-tab for the association statistics."
    })
    
    message("Finishing")
  }) # End univariable association
  
  # download data
  output$download.uni <- downloadHandler(
    filename = "Univariable_Association_Results.csv",
    content = function(file) {
      fwrite(isolate(values$res.univar), file)
    }
  )
  
  # Correlation Check -------------------------------------------------------
  # observe Slider and take values
  observeEvent(input$corr.cut.slider,{
    values$slider.input <- input$corr.cut.slider
  })
  
  # Pressing button
  observeEvent(input$corr.check.button, {
    
    # input
    annot.m <- isolate(values$annot.m)
    annot.c <- isolate(values$annot.c)
    dat <- isolate(values$dat)
    m.cols <- isolate(values$m.cols)
    c.cols <- isolate(values$c.cols)
    
    # get the correlation cutoff at the time of button clicking
    slider.input <- isolate(values$slider.input)
    
    validate(
      need(!any(dim(dat[, ..c.cols])==0), message = "Too many missings in data!")
    )
    
    high.corr <- show_highly_correlating(
      dataObject = dat,
      covariateColumns = c.cols,
      correlationCutoff = slider.input)
    
    # display message
    if(length(high.corr) != 0){
      corr.message <- paste0(
        "The covariates ",
        paste(high.corr, collapse = ", "), " correlate with r>", slider.input,
        ". Please select those you wish to exclude from the analysis.")
    } else {
      corr.message <- paste0(
        "None of the covariates correlate with r>", slider.input,
        ". No problem found. ")
    }
    output$high.corr <- renderText({corr.message})
    updateSelectInput(session, inputId = "exclude.corr.select", choices = c(NULL, high.corr)) #TAG#
    
    # create annotation in case no covariates correlate highly
    if(length(high.corr)==0){
      exclude.covariates <- c()
      annot.c[, correlation.exclusion := ifelse(covariate %in% c.cols, F, T)]
    }
    
    # plot annotation
    output$preview.corr.annot.c <- renderDataTable(annot.c, options = list(pageLength = 10))
    
    # update selector for later step
    updateSelectInput(session, inputId = "mandatory.inclusion.selecter", choices = c(NULL, c.cols), selected = NULL)
    
    # re-enter values into reactive
    values$annot.m <- annot.m
    values$annot.c <- annot.c
    values$dat <- dat
    values$m.cols <- m.cols
    values$c.cols <- c.cols
  })
  
  
  # Correlation Plot --------------------------------------------------------
  observeEvent(input$plot.corr.select, {
    req(values$dat)
    cohort <- input$plot.corr.select
    dat <- isolate(values$dat)
    c.cols <- isolate(values$c.cols)
    
    output$correlation.plot <- renderPlot(
      plot_correlation(
        data = dat,
        covariates = c.cols,
        cohort = cohort)
    )
  })
  
  # Exlude covariates -------------------------------------------------------
  # pressing exclude button
  observeEvent(input$exclude.corr.select,{
    
    # input
    annot.m <- isolate(values$annot.m)
    annot.c <- isolate(values$annot.c)
    dat <- isolate(values$dat)
    m.cols <- isolate(values$m.cols)
    c.cols <- isolate(values$c.cols)
    
    # get excludable covariates
    exclude.covariates <- isolate(input$exclude.corr.select)
    c.cols <- c.cols[!(c.cols %in% exclude.covariates)]
    
    # annotation table entry "correlation.exclusion"
    annot.c[, correlation.exclusion := ifelse(covariate %in% c.cols, F, T)]
    output$preview.corr.annot.c <- renderDataTable(annot.c, options = list(pageLength = 10))
    
    # output
    values$annot.m <- annot.m
    values$annot.c <- annot.c
    values$dat <- dat
    values$m.cols <- m.cols
    values$c.cols <- c.cols
  })
  
  # reset correlation exclusion
  observeEvent(input$corr.exclude.button,{
    
    # get the full c.cols and the changed annotation
    c.cols.original <- isolate(values$c.cols.original)
    annot.c <- isolate(values$annot.c)
    
    # reset correlation annotation
    annot.c[, correlation.exclusion := F]
    output$preview.corr.annot.c <- renderDataTable(annot.c, options = list(pageLength = 10))
    
    # write values
    values$c.cols <- c.cols.original
    values$annot.c <- annot.c
  })
  
  # Multivariable Association -----------------------------------------------
  observeEvent(input$multivar.assoc.button, {
    
    message("Starting")
    
    # input
    annot.c <- isolate(values$annot.c)
    dat <- isolate(values$dat)
    m.cols <- isolate(values$m.cols)
    c.cols <- isolate(values$c.cols)
    multiple.testing <- isolate(input$multivar.multiple.testing.correction.selecter)
    
    # compute multivariable association results
    res.multivar <- multivariable_assoc(dometab = m.cols,
                                        docovar = c.cols,
                                        data = dat)
    
    res.multivar <- generic_multiple_testing_correction(
      data = res.multivar,
      correctionMethod = multiple.testing)
    
    # friedman/wilcoxon test for r2 difference
    r2.test <- test_r2_distribution(dat = res.multivar,
                                    r2Col = "term.r.squared")

    # # add friedman/wilcoxon results to factor annotation
    r2.test.matched <- match(annot.c$covariate, r2.test$term)
    r2.test.join <- r2.test[(r2.test.matched), .SD, .SDcols = names(r2.test)[-1]]
    annot.c[, paste0("multivar.", names(r2.test.join)) := r2.test.join]
    
    # some formatting
    p.col <- tail(names(res.multivar), 1)
    sig.factors <- res.multivar[, .(sig = base::min(.SD) < 0.05,
                                    min.p = base::min(.SD)) ,by = .(term, cohort), .SDcols = p.col]
    annot.c[, tmp:=paste0(cohort, "_", covariate)]
    sig.factors[, tmp:=paste0(cohort, "_", term)]
    annot.c[match(sig.factors$tmp, tmp), multivariable.min.p := sig.factors$min.p]
    annot.c[match(sig.factors$tmp, tmp), multivariable.significant.p.0.05 := sig.factors$sig]
    annot.c$tmp <- NULL
    
    # built multivariable network
    output$network.multivar <- renderVisNetwork({
      
      # construct network plot
      network_plot(assocResults = copy(res.multivar),
                   rSquaredColumn = "term.r.squared",
                   pColumn = p.col,
                   cohort = input$network.multi.select,
                   hierarchicalNetwork = input$network.multi.hierarch)})
    
    # save results for output
    output$res.multivar <- renderDataTable(res.multivar, options = list(pageLength = 10))
    values$res.multivar <- res.multivar
    values$annot.c <- annot.c
    
    # placeholder
    multivar.description <- NULL
    
    # plot the unviaraiable results
    output$plot.multivar <- renderPlot({plot_univar(data = res.multivar, rSquaredCol = "term.r.squared")})
    
    # reformat results for heatmap
    multi.max.matrix <- make_matrices(st2 = res.multivar,
                                    r2Col = "term.r.squared",
                                    pCol = "p.hierarchical.bonferroni")
    
    # build heatmap of results
    output$heat.multivar <- renderPlot({
      custom_corrplot(multi.max.matrix$r2matrix,
                      method="color",
                      is.corr = F,
                      p.mat = multi.max.matrix$pvalmatrix,
                      insig = "label_sig", 
                      sig.level=0.05, 
                      pch.col = rgb(0,0,0,0.66), 
                      col = colorRampPalette(c("blue", "grey95", "red"))(20),
                      title = "Maximimum (signed) R2 across all cohorts",
                      mar=c(0, 0, 4, 0),
                      cl.length = 11
      )
    })
    
    # output methods text
    output$multivar.description <- renderText({
      validate(
        need(!is.null(multivar.description), message = "This is not yet available. See future Updates for a detailed description.")
      )
    })
    
    message("Finishing")
    
    # return success message
    output$multi.success.text <- renderText({
      "Multivariable associations calculated successfully. See the Results-tab for the association statistics."
    })
    
  }) # end multivariable association
  
  # download data
  output$download.multi <- downloadHandler(
    filename = "Multivariable_Association_Results.csv",
    content = function(file) {
      fwrite(isolate(values$res.multivar), file)
    }
  )
  
  # Covariable Selection ----------------------------------------------------
  # get parameters
  observe({
    values$r.squared.cutoff <- input$r.squared.cutoff.slider
    values$multiple.testing.correction <- input$multiple.testing.correction.selecter
    values$include.high.missings <- input$include.high.missings.checkbox
    values$missingness.cutoff <- input$missingness.cutoff.slider
    values$mandatory.inclusion <- input$mandatory.inclusion.selecter
  })
  
  # start calculations
  observeEvent(input$start.selection.button, {
    
    output$covar.select.start <- renderText("Crunching the numbers... Please be patient...")
    
    # input
    annot.m <- isolate(values$annot.m)
    annot.c <- isolate(values$annot.c)
    dat <- isolate(values$dat)
    m.cols <- isolate(values$m.cols)
    c.cols <- isolate(values$c.cols)
    
    # isolate parameters
    r.squared.cutoff <- isolate(values$r.squared.cutoff)
    multiple.testing.correction <- isolate(values$multiple.testing.correction)
    include.high.missings <- isolate(values$include.high.missings)
    missingness.cutoff <- isolate(values$missingness.cutoff)
    mandatory.inclusion <- isolate(values$mandatory.inclusion)
    
    if(length(mandatory.inclusion)==0){mandatory.inclusion<-NA}
    
    message(r.squared.cutoff)
    message(multiple.testing.correction)
    message(include.high.missings)
    message(missingness.cutoff)
    message(mandatory.inclusion)
    
    # start selection
    results <- find_relevant_covariates(
      covariateAnnotation = annot.c,
      metaboliteAnnotation = annot.m,
      dataObject = dat,
      rSquaredCutoff = r.squared.cutoff,
      includeHighMissings = include.high.missings,
      missingnessCutoff = missingness.cutoff,
      mandatoryInclusion = mandatory.inclusion,
      metaboliteColumns = m.cols,
      covariateColumns = c.cols
    )
    
    # re-assign output
    annot.c <- results$covariateAnnotation
    annot.m <- results$metaboliteAnnotation
    full.model.r.squared <- results$covariateModel
    res.selected <- results$testStatistics
    
    # compute the remaining statistics for the not-selected covariates
    res.remaining <- remaining_covariates(
      relevantCovariates = full.model.r.squared,
      CovariateColumns = c.cols,
      MetaboliteColumns = m.cols,
      dataObject = dat,
      rSquaredCutoff = r.squared.cutoff,
      highMissings = results$highMissings
    )
    
    # Apply multiple testing correction
    all.multi <- multiple_testing_correction(
      dataObjectSelected = res.selected,
      dataObjectRemaining = res.remaining,
      correctionMethod = multiple.testing.correction
    )
    
    #### PLOT ####
    plot <- plot_multivar(data = all.multi)
    output$multi.plot <- renderPlot(plot)
    
    # output
    values$all.multi <- all.multi
    values$full.model.r.squared <- full.model.r.squared
    values$annot.m <- annot.m
    values$annot.c <- annot.c
    values$dat <- dat
    values$m.cols <- m.cols
    values$c.cols <- c.cols
    
    message("Done!")
    output$covar.select.stop <- renderText(
      ifelse(
        all(
          is.na(
            full.model.r.squared$covariate
          )
        ), paste0("None of the covariates were relevant at the chosen cutoff of ",
                  expression("r"^2),
                  "> ",
                  isolate(values$r.squared.cutoff),
                  ". Please select a lower cutoff."),
        "Done! Please see the following tabs for your results"
        )
      )
    

    # Methods Description -----------------------------------------------------
    # placeholder
    univar.description <- NULL
    
    # output methods text
    output$selection.description <- renderText({
      validate(
        need(!is.null(univar.description), message = "This is not yet available. See future Updates for a detailed description.")
      )
    })
    
    # Output
    output$res.all.multi <- renderDataTable(isolate(values$all.multi), options = list(pageLength = 10))
    output$res.full.model <- renderDataTable(isolate(values$full.model.r.squared), options = list(pageLength = 10))
    output$res.annot.m <- renderDataTable(isolate(values$annot.m), options = list(pageLength = 10))
    output$res.annot.c <- renderDataTable(isolate(values$annot.c), options = list(pageLength = 10))
  })
  
  # Results -----------------------------------------------------------------
  # prepare data for download
  output$download.full.model.r.squared <- downloadHandler(
    filename = "Covariate_Selection.csv",
    content = function(file) {
      fwrite(isolate(values$full.model.r.squared), file)
    }
  )
  output$download.annot.c <- downloadHandler(
    filename = "Covariate_Annotation.csv",
    content = function(file) {
      fwrite(isolate(values$annot.c), file)
    }
  )
  output$download.annot.m <- downloadHandler(
    filename = "Metabolite_Annotation.csv",
    content = function(file) {
      fwrite(isolate(values$annot.m), file)
    }
  )
  output$download.dat <- downloadHandler(
    filename = "Preprocessed_Data.csv",
    content = function(file) {
      fwrite(isolate(values$dat), file)
    }
  )
  output$download.all.multi <- downloadHandler(
    filename = "Association_Results.csv",
    content = function(file) {
      fwrite(isolate(values$all.multi), file)
    }
  )
}
