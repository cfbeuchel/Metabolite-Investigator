# Define UI for application that draws a histogram
source(file = "functions/app_dependencies.R")

ui <-  navbarPage(
  title = "Analysis Steps",
  position = "static-top",
  
  #=== Panels ===#
  
  # Description ----
  tabPanel("Description", {
    sidebarLayout(
      sidebarPanel(width = 3,
                   helpText("This is a general description of the analysis steps.
                                Please follow the order as presented above.
                                Each step has the successful execution of the previous step as a prerequisite."),
                   tags$hr(),
                   helpText(strong("To ensure full functionality, please execute all steps in order."))
      ),
      mainPanel(width = 5,
                h3("Metabolite Investigator"),
                tags$hr(),
                p("Analysis and integration of data from high-throughput targeted metabolomics data has become a common tool in epidemiological research. However, no standardised workflow for preprocessing and subsequent analysis for metabolomics data from multiple large studies has been established. This Shiny-App facilitates the pre-processing, association analysis and factor selection for targeted metabolomics data from multiple study cohorts. We present a principled workflow tailored to the specific issues of metabolomics data, namely data skew, zero-inflation, (known) batch effects and the selection of a confounder model for the association with other '-omics' data sets. The descision for an analysis methodology was guided by a seperate simulation study to provide an approach with the least possible bias without compromising power."),
                tags$hr(),
                p("Upload and merge metabolite and factor data in steps 1.1 and 1.2. A short description of the workflow of these steps can be found below."),
                tags$hr(),
                h4("Step 1.1 - Upload of metabolite and factor data"),
                p("Please upload the metabolite and factor in the upload panel. The data is automatically uploaded and previewed. Please make sure you upload the factor data in the panel with the factor label and do the same for you metabolite data."),
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
  # 1.1 Data Upload ----
  tabPanel("1.1 Data Upload",{
    sidebarLayout(
      sidebarPanel(width = 3,
                   # use example data
                   helpText("Click here to load example data for a test run."),
                   actionButton(inputId = "use.example", label = "Use Example Data", icon = icon("play-circle")),
                   
                   # Horizontal line
                   tags$hr(),
                   
                   # dataTableOutput('mytable')
                   fileInput('input.covar', 'Choose factor-file to upload',
                             accept = c(
                               'text/csv',
                               'text/comma-separated-values',
                               'text/tab-separated-values',
                               'text/plain',
                               '.csv',
                               '.tsv'
                             )),
                   # Input: Select separator
                   radioButtons("sep.covar", "Separator",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t"),
                                selected = ","),
                   
                   # Input: Select quotes
                   radioButtons("quote.covar", "Quote",
                                choices = c(None = "",
                                            "Double Quote" = '"',
                                            "Single Quote" = "'"),
                                selected = '"'),
                   # Horizontal line
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
                   
                   # Input: Select separator
                   radioButtons("sep.metab", "Separator",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t"),
                                selected = ","),
                   
                   # Input: Select quotes 
                   radioButtons("quote.metab", "Quote",
                                choices = c(None = "",
                                            "Double Quote" = '"',
                                            "Single Quote" = "'"),
                                selected = '"')
      ), # END Sidebar panel
      
      mainPanel(width = 5,
                h3("Upload factor & metabolite files"),
                tags$hr(),
                verbatimTextOutput("preview.text"),
                verbatimTextOutput("metab.upload.success"),
                verbatimTextOutput("covar.upload.success"),
                tabsetPanel(type = "tabs",
                            tabPanel("Metabolites", DT::dataTableOutput("preview.metab")),
                            tabPanel("Factors", DT::dataTableOutput("preview.covar")),
                            tabPanel("Schematic", DT::dataTableOutput("preview.example"))
                )
      )
    )
  }),
  # 1.2 Data Prep ----
  tabPanel("1.2 Data Preparation",{
    sidebarLayout(
      sidebarPanel(width = 3,
                   # tags$style(HTML(".main-sidebar{width: 200px;}")),
                   helpText("Select the columns in your data that represent cohort, batch and sample ID.
                                Select your metabolite and/or factor columns in case there are other columns
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
                   selectInput("covar.id", "Select Factor Sample ID Column", choices = NULL), # no choices before uploading
                   checkboxInput("rest.covar",
                                 label = "Remaining Columns Are Factors",
                                 value = T,
                                 width = "400px"),
                   tags$hr(),
                   selectInput("metab.col", "Select All Metabolite Columns", choices = NULL, multiple = T), # no choices before uploading
                   selectInput("covar.col", "Select All Factor Columns", choices = NULL, multiple = T) # no choices before uploading
      ),
      mainPanel(width = 5,
                h3("Data merging and pre-processing"),
                tags$hr(),
                verbatimTextOutput("text.found.overlap"),
                verbatimTextOutput("text.merge.upper"),
                DT::dataTableOutput("data.merge")
      )
    )
  }),
  # 2 Data Pre-Pro ----
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
                                     p("Choose whether you want to pre-process your metabolite data. Pre-processing include three optional steps."),
                                     p("1) An outlier filter of 5*SD on log-transformed data."),
                                     p("2) A rank-based inverse-normal transformation, matching the rank of each measurement to the quantiles of a standard normal distribution with a mean of 0 and a standard deviation of 1."),
                                     p("3) A batch-correction of known technical batches via a non-parametric empirical Bayes method implemented in sva::ComBat.  Missing values are mean imputed for the analysis and missingness will be restored afterwards. Batches with only a single value entry will be removed from further analysis. Batches with low variance cannot be removed by ComBat and will thus be removed via a linear model. Method of batch adjustment will be entered in the annotation file."),
                                     tags$hr(),
                                     textOutput("prepro.description")),
                            tabPanel("Data", DT::dataTableOutput("prepro.data")),
                            tabPanel("Metabolite Annotation", DT::dataTableOutput("prepro.annot.m")),
                            tabPanel("Factor Annotation", DT::dataTableOutput("prepro.annot.c"))
                )
      )
    )
  }),
  # 3 Univariate Assoc ----
  tabPanel("3. Univariable Association",{
    sidebarLayout(
      sidebarPanel(width = 3,
                   helpText("Compute univariable association statistics for each metabolite and factor in each cohort.
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
                h3("Univariable Factor Association"),
                tags$hr(),
                verbatimTextOutput("uni.success.text"),
                tabsetPanel(type = "tabs",
                            tabPanel("Description", 
                                     tags$hr(),
                                     h4("Methods Description"),
                                     tags$hr(),
                                     p("In this step, each metabolite will be associated with each factor in in a univariate and univariable
                         linear model seperately in each cohort. Multiple testing correction for each cohort will be applied according to the selection in the drop down menu. Test statistics are downloadable as .csv file. Provided visualizations are a boxplot of the distributions of the explained variances for each factor in each cohort, a heatmap of the highest explained variance (signed by the direction of the effect estimate) of each factor for each metabolite over all cohorts and a (non-)hierarchical network with metabolites and factors as nodes and the maximum explained variance as edges. Further, the difference in the distribution of explained variance between cohorts will be quantified by Friedman test and when two distributions are compared via the Wilcoxon signed rank test. Benjamini-Hochberg correction for multiple testing will be applied afterwards."),
                                     tags$hr(),
                                     h5("Factor-Cohort Interaction Test"),
                                     p("In Case of more than one cohort data: Presence of univariable interaction terms of the cohort with each factor is analysed for each metabolite and displayed as a heatmap. For this, an interaction effect for the cohort ID is added to the univariable model. R-squared of the full model and the interaction term, as well as p-values of a Likelihood-Ratio test of the model withouth and with the interaction term are reported."),
                                     textOutput("univar.description")),
                            tabPanel("Boxplot", plotOutput("plot.univar")), #, width = "70%"
                            tabPanel("Heatmap", plotOutput("heat.univar")), #, width = "70%"
                            tabPanel("Network", visNetworkOutput("network.univar", width = "1200px",height = "800px")),
                            tabPanel("Results", DT::dataTableOutput("res.univar")),
                            tabPanel("Interaction Heatmap", plotOutput("heat.int.univar")), #, width = "70%"
                            tabPanel("Interaction Results", DT::dataTableOutput("res.int.univar"))
                )
      )
    )
  }),
  # 4 Correlation Check ----
  tabPanel("4. Correlation Check", {
    sidebarLayout(
      sidebarPanel(width = 3,
                   helpText("It is recommended to remove correlated factors from multivariable regression analysis. To support this task, we present correlation results for each cohort. After selecting a threshold for correlation, respective pairs of factors are reported and one of the factors can be manually removed using the drop-down menu"),
                   actionButton(inputId = "corr.check.button", label = "Check Correlation", icon = icon("play-circle")),
                   tags$hr(),
                   sliderInput("corr.cut.slider",
                               label = "Correlation threshold",
                               min = 0,
                               max = 0.99,
                               value = 0.75,
                               step = 0.01,
                               sep = "."),
                   tags$hr(),
                   helpText("Select a cohort to display the pairwise correlation of its factors:"),
                   selectInput(inputId = "plot.corr.select", label = "Display Correlation in Cohort", multiple = F, choices = NULL),
                   tags$hr(),
                   helpText("This panel displays factors correlated above the specified threshold to be optionally removed."),
                   selectInput(inputId = "exclude.corr.select", label = "Exclude Correlating Factors", multiple = T, choices = NULL),
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
                                     p("Highly correlating factors may cause multicolinearity-issues in the multivariable association.
You can check the pairwise Pearsons' correlation of each factor pair in each cohort. By selecting a maximum permissible
                         pairwise correlation via the input slider, the app lists factors that correlate above the selected threshold. You may then select and thus exclude any of the highly correlating factors from the menu below. The selection can be reset by pressing the `Reset Factor Exclusion`-button")
                            ),
                            tabPanel("Correlation", plotOutput("correlation.plot")), #  "700px" , width = "400px"
                            tabPanel("Factor Annotation", DT::dataTableOutput("preview.corr.annot.c"))
                )
      )  
    )
  }),
  # 5 Multivariable Assoc ----
  tabPanel("5. Multivariable Association", {
    sidebarLayout(
      sidebarPanel(width = 3,
                   helpText("Compute multivariable association statistics for each metabolite and all avaiable factors in each cohort.
                                You may also select a method for multiple testing correction from the drop-down menu."),
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
                h3("Multivariable Factor Association"),
                tags$hr(),
                verbatimTextOutput("multi.success.text"),
                tabsetPanel(type = "tabs",
                            tabPanel("Description",
                                     tags$hr(),
                                     h4("Methods Description"),
                                     tags$hr(),
                                     p("All factors not removed for high correlation will be fit against each metabolite in each cohort in a multivariable linear regression model. P-value-correction for multiple testing will be applied according to the selection from the drop-down menu. Visualizations similar to the univariable association step will be presented, with the difference that the partial explained variance of each factor depending on all the other factors will be provided in addition to the adjusted R2 of the full model. The partial-R2 of a factor is computed as the difference of the R2 of the model including and the R2 of a model excluding the respective factor."),
                                     tags$hr(),
                                     h5("Factor-Cohort Interaction Test"),
                                     p("In case of more than one cohort in the data: Similar to the cohort interaction test during the univariable association step of the interaction of each factor with the cohort is analysed for each metabolite in a multivariable linear model. For this, an interaction effect for the cohort ID is added to the multivariable model for each predictor. R-squared of the full model and the interaction term, as well as p-values of a Likelihood-Ratio test of the model withouth and with the interaction term are reported."),
                                     tags$hr(),
                                     textOutput("multivar.description")),
                            tabPanel("Boxplot", plotOutput("plot.multivar")),
                            tabPanel("Heatmap", plotOutput("heat.multivar")),
                            tabPanel("Network", visNetworkOutput("network.multivar", width = "1200px",height = "800px")),
                            tabPanel("Results", DT::dataTableOutput("res.multivar")),
                            tabPanel("Interaction Heatmap", plotOutput("heat.int.multivar")),
                            tabPanel("Interaction Results", DT::dataTableOutput("res.int.multivar"))
                )
      )
    )
  }),
  # 6. factor Selection ----
  tabPanel("6. Factor Selection", {
    sidebarLayout(
      sidebarPanel(width = 3,
                   conditionalPanel(
                     condition = "input.ChangeBasedOnThis == 'Plot/<br/>Settings' || input.ChangeBasedOnThis == 'Description/<br/>Settings'",
                     helpText("Press this button to start the factor selection"),
                     actionButton(inputId = "start.selection.button", label = "Factor Selection", icon = icon("play-circle")),
                     tags$hr(),
                     helpText("Specify the parameters of the factor selection. Necessary parameters can be very data specific."),
                     tags$hr(),
                     helpText("Decide on variance threshold each factor needs to meet in at least one cohort"),
                     sliderInput(inputId = "r.squared.cutoff.slider",
                                 label = "Minimum Explained Variance per factor",
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
                     helpText("Choose whether you want to include factors with a specified amount of missingness from the analysis or have them removed."),
                     checkboxInput(inputId = "exclude.high.missings.checkbox",
                                   label = "Exclude factors with high missingness",
                                   value = F),
                     sliderInput(inputId = "missingness.cutoff.slider",
                                 label = "Maximum Allowed Factor Missingness",
                                 min = 0,
                                 max = 1,
                                 value = 0.1,
                                 step = 0.01,
                                 sep = "."),
                     tags$hr(),
                     helpText("Select factors that should be included mandatorily.
                                   Use this to select factors you want to include in the selection results regardless
                                   of their explained variance."),
                     selectInput(inputId = "mandatory.inclusion.selecter",
                                 label = "Mandatory Inclusion of Factors:",
                                 choices = NULL, multiple = T, selected = NULL)),
                   # sidebarPanel(width = 3,
                   conditionalPanel(
                     condition = "input.ChangeBasedOnThis != 'Plot/<br/>Settings' && input.ChangeBasedOnThis != 'Description/<br/>Settings'",
                     helpText("Download the results of the analysis here:"),
                     tags$hr(),
                     helpText("Download Relevant Factors:"),
                     downloadButton("download.full.model.r.squared", "Download", class = "butt"),
                     tags$hr(),
                     helpText("Download Association Results:"),
                     downloadButton("download.all.multi", "Download", class = "butt"),
                     tags$hr(),
                     helpText("Download Metabolite Annotation:"),
                     downloadButton("download.annot.m", "Download", class = "butt"),
                     tags$hr(),
                     helpText("Download Factor Annotation:"),
                     downloadButton("download.annot.c", "Download", class = "butt")
                   ) # conditionalPanel
      ), # sidebarPanel
      mainPanel(width = 5,
                h3("Factor Selection"),
                tags$hr(),
                verbatimTextOutput("covar.select.stop"),
                tabsetPanel(id = "ChangeBasedOnThis", 
                            tabPanel(HTML("Description/<br/>Settings"), 
                                     tags$hr(),
                                     h4("Methods Description"),
                                     tags$hr(),
                                     p("Search for factors explaining a threshold-amount of variation in your data, e.g. for use as a consensus factor model for subsequent regression analysis. This method removes factors not explaining a set amount of variance in at least one metabolite and cohort. It is possible to mandatorily include a set of factors as that will be selected irrespective of their explained variance (and/or missingness). This may be useful in case certain factors are known to influence metabolites. Downloadable results include metabolite and factor annotations, partial explained variances of all factors."),
                                     tags$hr(),
                                     textOutput("selection.description")),
                            tabPanel(HTML("Plot/<br/>Settings"),
                                     h4("Factor Selection"),
                                     tags$hr(),
                                     plotOutput("multi.plot", hover = T, width = "70%")
                            ),
                            tabPanel(HTML("Relevant<br/>Factors"),
                                     h4("Your relevant factors are:"),
                                     helpText("This table displays the maximum-partial-r-squared per factor and cohort."),
                                     tags$hr(),
                                     DT::dataTableOutput("res.full.model")
                            ),
                            tabPanel(HTML("Association<br/>Statistics"), 
                                     h4("Association Statistics"),
                                     tags$hr(),
                                     DT::dataTableOutput("res.all.multi")),
                            tabPanel(HTML("Metabolite<br/>Annotation"),
                                     h4("Your Metabolite Annotation"),
                                     tags$hr(),
                                     DT::dataTableOutput("res.annot.m")),
                            tabPanel(HTML("Factor<br/>Annotation"),
                                     h4("Your Factor Annotation"),
                                     tags$hr(),
                                     DT::dataTableOutput("res.annot.c"))
                )
      ) # mainPanel
    ) # sidebarLayout
  })
)
