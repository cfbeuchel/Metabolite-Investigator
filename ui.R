# Define UI for application that draws a histogram
library("visNetwork")

ui <-  navbarPage(
  title = "Analysis Steps",
  position = "static-top",
  theme = shinythemes::shinytheme("united"),
  
  #=== Panels ===#
  
  tabPanel("Description", {
    sidebarLayout(
      sidebarPanel(width = 3,
                   helpText("This is a general description of the analysis steps taken in this application.
                                The analysis steps are executed in the order they are presented in in the tabs above.
                                Each step has the successful execution of the previous step as a prerequisite."),
                   tags$hr(),
                   helpText(strong("To ensure full functionality, please execute all steps in order."))
      ),
      mainPanel(width = 5,
                h3("Metabolite Investigator"),
                tags$hr(),
                p("Analysis and integration of data from high-throughput targeted metabolomics data has become a common tool in epidemiological research. However, no standardised workflow for preprocessing and subsequent analysis for metabolomics data from multiple large studies has been established. This Shiny-App facilitates the pre-processing, association analysis and covariable selection for targeted metabolomics data from multiple study cohorts. We present a principled workflow tailored to the specific issues of metabolomics data, namely data skew, zero-inflation, (known) batch effects and the selection of a confounder model for the association with other '-omics' data sets. The descision for an analysis methodology was guided by a seperate simulation study to provide an approach with the least possible bias without compromising power."),
                tags$hr(),
                p("Upload and merge metabolite and covariate data in steps 1.1 and 1.2. A short description of the workflow of these steps can be found below."),
                tags$hr(),
                h4("Step 1.1 - Upload of metabolite and covariate data"),
                p("Please upload the metabolite and covariate in the upload panel. The data is automatically uploaded and previewed. Please make sure you upload the covariate data in the panel with the covariate label and do the same for you metabolite data."),
                # tags$hr(),
                h4("Step 1.2 - Merging of data"),
                p("By selecting the sample ID columns for your data as well as the batch and cohort ID
                         from the provided drop-down menus you allow the correct assignment of these columns
                         for the merging of the data and the subsequent analysis. After selecting the right 
                         columns, press the 'Merge' button to preview and ready the data object for analysis.")#,
                # tags$hr(),
                # h4("Step 3 - Pre-processing of data"),
                # tags$hr(),
                # h4("Step 4 - Univariable Association"),
                # tags$hr(),
                # h4("Step 5 - Check Correlation"),
                # tags$hr(),
                # h4("Step 5 - Multivariable Association"),
                # tags$hr(),
                # h4("Step 6 - Multivariable Covariate Selection")
      )
    )
  }),
  tabPanel("1.1 Data Upload",{
    sidebarLayout(
      sidebarPanel(width = 3,
                   # use example data
                   helpText("Click here to load example data for a test run."),
                   actionButton(inputId = "use.example", label = "Use Example Data", icon = icon("play-circle")),
                   
                   # Horizontal line ----
                   tags$hr(),
                   
                   # dataTableOutput('mytable')
                   fileInput('input.covar', 'Choose covariate-file to upload',
                             accept = c(
                               'text/csv',
                               'text/comma-separated-values',
                               'text/tab-separated-values',
                               'text/plain',
                               '.csv',
                               '.tsv'
                             )),
                   # Input: Select separator ----
                   radioButtons("sep.covar", "Separator",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t"),
                                selected = ","),
                   
                   # Input: Select quotes ----
                   radioButtons("quote.covar", "Quote",
                                choices = c(None = "",
                                            "Double Quote" = '"',
                                            "Single Quote" = "'"),
                                selected = '"'),
                   # Horizontal line ----
                   tags$hr(),
                   
                   # dataTableOutput('mytable')
                   fileInput('input.metab', 'Choose metabolite-file to upload',
                             accept = c(
                               'text/csv',
                               'text/comma-separated-values',
                               'text/tab-separated-values',
                               'text/plain',
                               '.csv',
                               '.tsv'
                             )),
                   
                   # Input: Select separator ----
                   radioButtons("sep.metab", "Separator",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t"),
                                selected = ","),
                   
                   # Input: Select quotes ----
                   radioButtons("quote.metab", "Quote",
                                choices = c(None = "",
                                            "Double Quote" = '"',
                                            "Single Quote" = "'"),
                                selected = '"')
      ), # END Sidebar panel
      
      mainPanel(width = 5,
                h3("Upload covariate & metabolite files"),
                tags$hr(),
                verbatimTextOutput("preview.text"),
                verbatimTextOutput("metab.upload.success"),
                verbatimTextOutput("covar.upload.success"),
                tabsetPanel(type = "tabs",
                            tabPanel("Metabolites", dataTableOutput("preview.metab")),
                            tabPanel("Covariates", dataTableOutput("preview.covar")),
                            tabPanel("Schematic", dataTableOutput("preview.example"))
                )
      )
    )
  }),
  tabPanel("1.2 Data Preparation",{
    sidebarLayout(
      sidebarPanel(width = 3,
                   # tags$style(HTML(".main-sidebar{width: 200px;}")),
                   helpText("Select the columns in your data that represent cohort, batch and sample ID.
                                Select your metabolite and/or covariate columns in case there are other columns
                                in your data you do not want to analyse. If this is finished, press the merge button below."),
                   actionButton("merge.button", "Merge Data", icon = icon("play-circle")),
                   tags$hr(),
                   # selector for input
                   selectInput("cohort.col", "Select Metabolite Cohort ID Column", choices = NULL), # no choices before uploading
                   selectInput("batch.col", "Select Metabolite Batch ID Column", choices = NULL), # no choices before uploading
                   selectInput("metab.id", "Select Metabolite Sample ID Column", choices = NULL), # no choices before uploading
                   checkboxInput("rest.metab",
                                 label = "Remaining Columns Are Metabolites",
                                 value = T,
                                 width = "400px"),
                   tags$hr(),
                   selectInput("covar.id", "Select Covariate Sample ID Column", choices = NULL), # no choices before uploading
                   checkboxInput("rest.covar",
                                 label = "Remaining Columns Are Covariates",
                                 value = T,
                                 width = "400px"),
                   tags$hr(),
                   selectInput("metab.col", "Select All Metabolite Columns", choices = NULL, multiple = T), # no choices before uploading
                   selectInput("covar.col", "Select All Covariate Columns", choices = NULL, multiple = T) # no choices before uploading
      ),
      mainPanel(width = 5,
                h3("Data merging and pre-processing"),
                tags$hr(),
                verbatimTextOutput("text.found.overlap"),
                verbatimTextOutput("text.merge.upper"),
                dataTableOutput("data.merge")
      )
    )
  }),
  tabPanel("2. Data Pre-Processing", {
    sidebarLayout(
      sidebarPanel(width = 3,
                   helpText("Press this button to pre-process your data"),
                   actionButton(inputId = "button.prepro", label = "Pre-Processing", icon = icon("play-circle")),
                   tags$hr(),
                   helpText("Please choose whether you want the metabolites pre-processed or not:"),
                   checkboxInput("button.choose.prepro",
                                 "Pre-Process metabolites?",
                                 value = T),
                   helpText("Please select the pre-processing steps you would like to include:"),
                   checkboxGroupInput(inputId = "button.prepro.steps",
                                      label = "Included pre-processing steps",
                                      choices = list(
                                        "Outlier Filter of 5*SD",
                                        "Inverse-Normal Transformation",
                                        "Batch Adjustment"
                                      ),
                                      selected = list(
                                        "Outlier Filter of 5*SD",
                                        "Inverse-Normal Transformation",
                                        "Batch Adjustment"
                                      )),
                   tags$hr(),
                   helpText("Download Preprocessed Data:"),
                   downloadButton("download.dat", "Download", class = "butt")
      ), 
      mainPanel(width = 5,
                h3("Data pre-processing"),
                tags$hr(),
                verbatimTextOutput("prepro.success"),
                tabsetPanel(type = "tabs",
                            tabPanel("Description",
                                     tags$hr(),
                                     h4("Methods Description"),
                                     tags$hr(),
                                     p("Choose whether you want to preprocess your metabolite data. Preprocessing steps include three steps."),
                                     p("1) An outlier filter of 5*SD on log-transformed data."),
                                     p("2) A rank-based inverse-normal transformation, matching the rank of each measurement to the quantiles of a standard normal distribution with a mean of 0 and a standard deviation of 1."),
                                     p("3) A batch-correction of known technical batches via a non-parametric empirical Bayes method implemented in sva::ComBat.  Missing values are mean imputed for the analysis and missingness will be restored afterwards. Batches with only a single value entry will be removed from further analysis. Batches with low variance cannot be removed by ComBat and will thus be removed via a linear model. Method of batch adjustment will be entered in the annotation file."),
                                     tags$hr(),
                                     textOutput("prepro.description")),
                            tabPanel("Data", dataTableOutput("prepro.data")),
                            tabPanel("Metabolite Annotation", dataTableOutput("prepro.annot.m")),
                            tabPanel("Covariate Annotation", dataTableOutput("prepro.annot.c"))
                )
      )
    )
  }),
  tabPanel("3. Univariable Association",{
    sidebarLayout(
      sidebarPanel(width = 3,
                   helpText("Compute univariable association statistics for each metabolite and covariate in each cohort.
                                You can also add multiple testing correction from the drop-down menu."),
                   actionButton(inputId =  "univar.assoc.button", label =  "Univariable Association", icon = icon("play-circle")),
                   tags$hr(),
                   selectInput(inputId = "univar.multiple.testing.correction.selecter",
                               label = "Correction for multiple testing",
                               choices = list("Bonferroni" = "bonferroni",
                                              "FDR" = "fdr",
                                              "hierarchical FDR (Benjamini Bogomolov)" = "hierarchical.bb",
                                              "hierarchical Bonferroni" = "hierarchical.bf"),
                               selected = "hierarchical.bf"),
                   tags$hr(),
                   selectInput(inputId = "network.uni.select",
                               label = "Display bi-partite Network for",
                               choices = NULL),
                   checkboxInput(inputId = "network.uni.hierarch",
                                 label = "Hierarchical Network",
                                 value = T),
                   tags$hr(),
                   helpText("Download Univariable Association Results:"),
                   downloadButton("download.uni", "Download", class = "butt"),
                   tags$hr(),
                   helpText("Download Univariable Factor-Cohort Interaction Results:"),
                   downloadButton("download.int.uni", "Download", class = "butt")
      ),
      mainPanel(width = 5,
                h3("Univariable Covariate Association"),
                tags$hr(),
                verbatimTextOutput("uni.success.text"),
                tabsetPanel(type = "tabs",
                            tabPanel("Description", 
                                     tags$hr(),
                                     h4("Methods Description"),
                                     tags$hr(),
                                     p("In this step, each metabolite will be associated with each covariate in in a univariate and univariable
                         linear model seperately in each cohort. Multiple testing correction for each cohort will be applied according to the selection in the drop down menu. Test statistics are downloadable as .csv file. Provided visualizations are a boxplot of the distributions of the explained variances for each factor in each cohort, a heatmap of the highest explained variance (signed by the direction of the effect estimate) of each factor for each metabolite over all cohorts and a (non-)hierarchical network with metabolites and factors as nodes and the maximum explained variance as edges. Further, the difference in the distribution of explained variance between cohorts will be quantified by Friedman test and when two distributions are compared via the Wilcoxon signed rank test. Benjamini-Hochberg correction for multiple testing will be applied afterwards."),
                                     tags$hr(),
                                     h5("Cohort Interaction Test"),
                                     p("Presense of univariable interaction terms of the cohort with each factor is analysed for each metabolite. "),
                                     textOutput("univar.description")),
                            tabPanel("Boxplot", plotOutput("plot.univar", width = "70%")),
                            tabPanel("Heatmap", plotOutput("heat.univar", width = "70%")),
                            tabPanel("Network", visNetworkOutput("network.univar")),
                            tabPanel("Results", dataTableOutput("res.univar")),
                            tabPanel("Interaction Heatmap", plotOutput("heat.int.univar", width = "70%")),
                            tabPanel("Interaction Results", dataTableOutput("res.int.univar"))
                )
      )
    )
  }),
  tabPanel("4. Correlation Check", {
    sidebarLayout(
      sidebarPanel(width = 3,
                   helpText("You can remove highly correlating (Pairwise Pearsons Correlation Coefficient) covariates from further analysis.
                                Firstly, checks for which of the factors correlate highly are performed and presented in a correlation matrix for each cohort.
                                Afterwards, please select cutoff, representing both positive and negative correlation
                                (0 = absolutely no correlation allowed; 1 = perfect positive/negative correlation tolerated). Factors correlating above the selected thresold can be removed from the multivariable analysis via the drop-down menu."),
                   actionButton(inputId = "corr.check.button", label = "Check Correlation", icon = icon("play-circle")),
                   tags$hr(),
                   sliderInput("corr.cut.slider",
                               label = "Correlation cutoff",
                               min = 0,
                               max = 0.99,
                               value = 0.75,
                               step = 0.01,
                               sep = "."),
                   tags$hr(),
                   helpText("You can plot the pairwise correlation of the covariates for each cohort here:"),
                   selectInput(inputId = "plot.corr.select", label = "Display Correlation in Cohort", multiple = F, choices = NULL),
                   tags$hr(),
                   helpText("This panel automatically displays only covariates that correlate above the specified cutoff. This step can be omitted."),
                   selectInput(inputId = "exclude.corr.select", label = "Exclude Correlating Covariates", multiple = T, choices = NULL),
                   actionButton(inputId = "corr.exclude.button", label = "Reset Factor Exclusion", icon = icon("redo"))
      ),
      mainPanel(width = 5,
                h3("Correlation Check"),
                tags$hr(),
                verbatimTextOutput("high.corr"),
                tabsetPanel(type = "tabs",
                            tabPanel("Description", 
                                     tags$hr(),
                                     h4("Methods Description"),
                                     tags$hr(),
                                     p("Highly correlated covariates may cause multicolinearity-issues in the multivariable association.
You can check the pairwise Pearsons' correlation of each covariate pair in each cohort via a correlation
                         matrix plot for each cohort, accessible through the drop-down menu. By selecting a maximum allowable
                         correlation via the input slider, the app returns each covariate that correlate above the selected threshold
                         by pressing the button. You may then select and thus exclude any of the highly correlating factors from the menu below and exclude 
                         them from further analysis. The selection can be reset by pressing the `Reset Factor Exclusion`-button")
                            ),
                            tabPanel("Correlation", plotOutput("correlation.plot", width = "400px")), #  "700px" 
                            tabPanel("Covariate Annotation", dataTableOutput("preview.corr.annot.c"))
                )
      )  
    )
  }),
  tabPanel("5. Multivariable Association", {
    sidebarLayout(
      sidebarPanel(width = 3,
                   helpText("Compute multivariable association statistics for each metabolite and all avaiable covariates in each cohort.
                                You can also add multiple testing correction from the drop-down menu."),
                   actionButton(inputId =  "multivar.assoc.button", label =  "Multivariable Association", icon = icon("play-circle")),
                   tags$hr(),
                   selectInput(inputId = "multivar.multiple.testing.correction.selecter",
                               label = "Correction for multiple testing",
                               choices = list("Bonferroni" = "bonferroni",
                                              "FDR" = "fdr",
                                              "hierarchical FDR (Benjamini Bogomolov)" = "hierarchical.bb",
                                              "hierarchical Bonferroni" = "hierarchical.bf"),
                               selected = "hierarchical.bf"),
                   tags$hr(),
                   selectInput(inputId = "network.multi.select",
                               label = "Display bi-partite Network for",
                               choices = NULL),
                   checkboxInput(inputId = "network.multi.hierarch",
                                 label = "Hierarchical Network",
                                 value = T),
                   tags$hr(),
                   helpText("Download Multivariable Association Results:"),
                   downloadButton("download.multi", "Download", class = "butt"),
                   helpText("Download Multivariable Factor-Cohort Interaction Association Results:"),
                   downloadButton("download.int.multi", "Download", class = "butt")
      ),
      mainPanel(width = 5,
                h3("Multivariable Covariate Association"),
                tags$hr(),
                verbatimTextOutput("multi.success.text"),
                tabsetPanel(type = "tabs",
                            tabPanel("Description",
                                     tags$hr(),
                                     h4("Methods Description"),
                                     tags$hr(),
                                     p("All factors not removed for high correlation will be regressed against each metabolite in each cohort in a multivariable linear regression model. Correction for multiple testing will be applied according to the selection from the drop-down menu. Visualizations are the same as in the univariable association step with the difference that the partial explained variance of each factor depending on all the other factors will be provided in addition to the adjusted R2 of the full model. The partial-R2 of a factor is computed as the difference of the R2 of the model including and the R2 of a model excluding the factor in question."),
                                     tags$hr(),
                                     textOutput("multivar.description")),
                            tabPanel("Boxplot", plotOutput("plot.multivar", width = "70%")),
                            tabPanel("Heatmap", plotOutput("heat.multivar", width = "70%")),
                            tabPanel("Network", visNetworkOutput("network.multivar")),
                            tabPanel("Results", dataTableOutput("res.multivar")),
                            tabPanel("Interaction Heatmap", plotOutput("heat.int.multivar", width = "70%")),
                            tabPanel("Interaction Results", dataTableOutput("res.int.multivar"))
                )
      )
    )
  }),
  tabPanel("6. Covariate Selection", {
    sidebarLayout(
      sidebarPanel(width = 3,
                   conditionalPanel(
                     condition = "input.ChangeBasedOnThis == 'Plot/<br/>Settings' || input.ChangeBasedOnThis == 'Description/<br/>Settings'",
                     helpText("Press this button to start the covariable selection"),
                     actionButton(inputId = "start.selection.button", label = "Covariate Selection", icon = icon("play-circle")),
                     tags$hr(),
                     helpText("Specify the parameters of the covariate selection. Necessary parameters can be very data specific."),
                     tags$hr(),
                     helpText("Decide on variance cutoff each covariate needs to fulfill in at least one cohort"),
                     sliderInput(inputId = "r.squared.cutoff.slider",
                                 label = "Minimum Explained Variance per covariate",
                                 min = 0,
                                 max = 0.2,
                                 value = 0.025,
                                 step = 0.001,
                                 sep = "."),
                     tags$hr(),
                     selectInput(inputId = "multiple.testing.correction.selecter",
                                 label = "Correction for multiple testing",
                                 choices = list("Bonferroni" = "bonferroni",
                                                "FDR" = "fdr",
                                                "hierarchical FDR (Benjamini Bogomolov)" = "hierarchical.bb",
                                                "hierarchical Bonferroni" = "hierarchical.bf"),
                                 selected = "hierarchical.bf"),
                     tags$hr(),
                     helpText("Choose whether you want to include covariates with a specified amount of missingness from the analysis or have them removed."),
                     checkboxInput(inputId = "exclude.high.missings.checkbox",
                                   label = "Exclude covariates with high missingness",
                                   value = F),
                     sliderInput(inputId = "missingness.cutoff.slider",
                                 label = "Maximum Allowed Covariate Missingness",
                                 min = 0,
                                 max = 1,
                                 value = 0.1,
                                 step = 0.01,
                                 sep = "."),
                     tags$hr(),
                     helpText("Select covariates that should be in the selection results mandatorily.
                                   Use this to select factors that you know are important and you want to include in a confounder model regardless
                                   of the variance it explains."),
                     selectInput(inputId = "mandatory.inclusion.selecter",
                                 label = "Mandatory Inclusions",
                                 choices = NULL, multiple = T, selected = NULL)),
                   # sidebarPanel(width = 3,
                   conditionalPanel(
                     condition = "input.ChangeBasedOnThis != 'Plot/<br/>Settings' && input.ChangeBasedOnThis != 'Description/<br/>Settings'",
                     helpText("Download the results of the analysis here:"),
                     tags$hr(),
                     helpText("Download Relevant Covariates:"),
                     downloadButton("download.full.model.r.squared", "Download", class = "butt"),
                     tags$hr(),
                     helpText("Download Association Results:"),
                     downloadButton("download.all.multi", "Download", class = "butt"),
                     tags$hr(),
                     helpText("Download Metabolite Annotation:"),
                     downloadButton("download.annot.m", "Download", class = "butt"),
                     tags$hr(),
                     helpText("Download Covariate Annotation:"),
                     downloadButton("download.annot.c", "Download", class = "butt")
                   ) # conditionalPanel
      ), # sidebarPanel
      mainPanel(width = 5,
                h3("Covariate Selection"),
                tags$hr(),
                verbatimTextOutput("covar.select.stop"),
                tabsetPanel(id = "ChangeBasedOnThis", 
                            tabPanel(HTML("Description/<br/>Settings"), 
                                     tags$hr(),
                                     h4("Methods Description"),
                                     tags$hr(),
                                     p("This steps allowes the search for covariates explaining a certain amount of variation in your data, e.g. for use as a consensus covariate model for subsequent analysis via backwards selection. This method removes factors not explaining a set amount of variance in at least one metabolite. This allows for conservative analysis of data via the maximum partial explained variance per factor per cohort. The cutoff can be set manually or the default may be chosen. Additionally, it is possible to include a set of factors as mandatory facors that will be classified as relevent irrespective of their explained variance. This may be useful in case certain factors are known to influence predictors. Downloadable results include metabolite and covariate annotations, partial explained variances of the relevant as well as the irrelevant (according to the set threshold) covariates."),
                                     tags$hr(),
                                     textOutput("selection.description")),
                            tabPanel(HTML("Plot/<br/>Settings"),
                                     h4("Covariate Selection"),
                                     tags$hr(),
                                     plotOutput("multi.plot", hover = T, width = "70%")
                            ),
                            tabPanel(HTML("Relevant<br/>Covariates"),
                                     h4("Your relevant covariates are:"),
                                     helpText("This table displays the maximum-partial-r-squared found per covariate per cohort that passed the set threshold."),
                                     tags$hr(),
                                     dataTableOutput("res.full.model")
                            ),
                            tabPanel(HTML("Association<br/>Statistics"), 
                                     h4("Association Statistics"),
                                     tags$hr(),
                                     dataTableOutput("res.all.multi")),
                            tabPanel(HTML("Metabolite<br/>Annotation"),
                                     h4("Your Metabolite Annotation"),
                                     tags$hr(),
                                     dataTableOutput("res.annot.m")),
                            tabPanel(HTML("Covariate<br/>Annotation"),
                                     h4("Your Covariate Annotation"),
                                     tags$hr(),
                                     dataTableOutput("res.annot.c"))
                )
      ) # mainPanel
    ) # sidebarLayout
  })
)
