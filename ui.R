# Define UI for application that draws a histogram
ui <- navbarPage("Analysis Steps",
                 position = "static-top",
                 #TAG# Download button coloring
                 tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: white;}")),
                 # Panels
                 tabPanel("Description", {
                   sidebarLayout(
                     sidebarPanel(
                       helpText("This is a general description of the analysis steps taken in this application.
                                The analysis steps are executed in the order they are presented in in the tabs above.
                                Each step has the successful execution of the previous step as a prerequisite."),
                       tags$hr(),
                       helpText(strong("To ensure full functionality, please execute all steps in order."))
                     ),
                     mainPanel(
                       h3("Analysis Steps"),
                       tags$hr(),
                       h4("Step 1 - Upload of metabolite and covariate data"),
                       p("Please upload the metabolite and
                         covariate in the upload panel. The data is automatically uploaded and previewed.
                         Please make sure you upload the covariate data in the panel with the covariate label
                         and do the same for you metabolite data."),
                       h4("Step 2 - Merging of data"),
                       p("By selecting the sample ID columns for your data as well as the batch and cohort ID
                         from the provided drop-down menus you allow the correct assignment of these columns
                         for the merging of the data and the subsequent analysis. After selecting the right 
                         columns, press the 'Merge' button to preview and ready the data object for analysis."),
                       h4("Step 3 - Pre-processing of data"),
                       p("Choose whether you want to preprocess your metabolite data. Preprocessing steps include
                         an outlier filter of 5xSD, an inverse-normal transformation and a batch-correction via
                         sva::ComBat()"),
                       h4("Step 4 - Univariable Association"),
                       p("Each metabolite will be associated with each covariate in in a univariate and univariable,
                         linear model in each cohort. Test statistics are downloadable as .csv file "),
                       h4("Step 5 - Check Correlation"),
                       p("Highly correlated covariates may cause multicolinearity-issues in the multivariable association.
You can check the pairwise Pearsons' correlation of each covariate pair in each cohort via a correlation
                         matrix plot for each cohort, accessible through the drop-down menu. By selecting a maximum allowable
                         correlation via the input slider, the app returns each covariate that correlate above the selected threshold
                         by pressing the button. You may then select any of the highly correlating from the menu below and exclude 
                         them from further analysis by pressing the 'Exclude'-button."),
                       h4("Step 5 - Multivariable Association"),
                       p("Multivariable model fit statistics using all (non-excluded) covariates as predictors for each metabolite.
                         Results may be downloaded via the button."),
                       h4("Step 6 - Multivariable Covariate Selection"),
                       p("Search for covariates explaining a certain amount of variation in your data, e.g. for use as a consensus 
covariate model for subsequent analysis. This method allows for conservative
                         analysis of data via the maximum partial explained variance per factor per cohort. The cutoff can be set manually
                         or the default may be chosen. Additionally, it is possible to include a set of factors as mandatory facors that
                         will be classified as relevent irrespective of their explained variance. This may be useful in case 
                         certain factors are known to influence predictors. Downloadable results include metabolite and covariate annotations,
                         partial explained variances of the relevant as well as the irrelevant (according to the set threshold)
                         covariates.")
                     )
                   )
                 }),
                 tabPanel("1.1 Data Upload",{
                   sidebarLayout(
                     sidebarPanel(
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
                     
                     mainPanel(
                       h3("Upload covariate & metabolite files"),
                       tags$hr(),
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
                     sidebarPanel(
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
                     mainPanel(
                       h3("Data merging and pre-processing"),
                       tags$hr(),
                       verbatimTextOutput("text.merge.upper"),
                       verbatimTextOutput("text.merge.lower"),
                       dataTableOutput("data.merge")
                     )
                   )
                 }),
                 tabPanel("2. Data Pre-Processing", {
                   sidebarLayout(
                     sidebarPanel(
                       helpText("Press this button to pre-process your data"),
                       actionButton(inputId = "button.prepro", label = "Pre-Processing", icon = icon("play-circle")),
                       tags$hr(),
                       helpText("Please choose whether you want the metabolites pre-processed or not:"),
                       checkboxInput("button.choose.prepro",
                                     "Pre-Process metabolites?",
                                     value = T)
                     ), 
                     mainPanel(
                       h3("Data pre-processing"),
                       tags$hr(),
                       verbatimTextOutput("prepro.success"),#TAG#
                       tabsetPanel(type = "tabs",
                                   tabPanel("Description",
                                            h4("Methods Description"),
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
                     sidebarPanel(
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
                       helpText("Download Univariable Association Results:"),
                       downloadButton("download.uni", "Download", class = "butt")
                     ),
                     mainPanel(
                       h3("Univariable Covariate Association"),
                       tags$hr(),
                       tabsetPanel(type = "tabs",
                                   tabPanel("Description", 
                                            h4("Methods Description"),
                                            tags$hr(),
                                            textOutput("univar.description")),
                                   tabPanel("Plot", plotOutput("plot.univar", width = "70%")),
                                   tabPanel("Results", dataTableOutput("res.univar"))
                       )
                     )
                   )
                 }),
                 tabPanel("4. Correlation Check", {
                   sidebarLayout(
                     sidebarPanel(
                       helpText("You can remove highly correlating (Pearsons Correlation Coefficient) covariates from further analysis.
                                First, we can check, which of the factors correlate highly.
                                Therefore, please select cutoff, representing both positive and negative correlation
                                (0 = absolutely no correlation allowed; 1 = perfect positive/negative correlation tolerated)"),
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
                       actionButton(inputId = "corr.exclude.button", label = "Exclude Factors", icon = icon("play-circle"))
                     ),
                     mainPanel(
                       h3("Correlation Check"),
                       tags$hr(),
                       verbatimTextOutput("high.corr"),
                       tabsetPanel(type = "tabs",
                                   tabPanel("Correlation", plotOutput("correlation.plot",
                                                                      width = "auto",
                                                                      height = "700px")),
                                   tabPanel("Covariate Annotation", dataTableOutput("preview.corr.annot.c"))
                       )
                     )  
                   )
                 }),
                 tabPanel("5. Multivariable Association", {
                   sidebarLayout(
                     sidebarPanel(
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
                       helpText("Download Multivariable Association Results:"),
                       downloadButton("download.multi", "Download", class = "butt")
                     ),
                     mainPanel(
                       h3("Multivariable Covariate Association"),
                       tags$hr(),
                       tabsetPanel(type = "tabs",
                                   tabPanel("Description",
                                            h4("Methods Description"),
                                            tags$hr(),
                                            textOutput("multivar.description")),
                                   tabPanel("Results", dataTableOutput("res.multivar"))
                       )
                     )
                   )
                 }),
                 tabPanel("6. Covariate Selection", {
                   sidebarLayout(
                     sidebarPanel(
                       conditionalPanel(
                         condition = "input.ChangeBasedOnThis == 'Plot' || input.ChangeBasedOnThis == 'Description'",
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
                         checkboxInput(inputId = "include.high.missings.checkbox",
                                       label = "Include covariates with high missings?",
                                       value = T),
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
                         inputPanel(
                           selectInput(inputId = "mandatory.inclusion.selecter",
                                       label = "Mandatory Inclusions",
                                       choices = NULL, multiple = T, selected = NULL))
                       ),
                       # sidebarPanel(
                       conditionalPanel(
                         condition = "input.ChangeBasedOnThis != 'Plot' || input.ChangeBasedOnThis != 'Description'",
                         helpText("Download the results of the analysis here:"),
                         tags$hr(),
                         helpText("Download Relevant Covariates:"),
                         downloadButton("download.full.model.r.squared", "Download", class = "butt"),
                         tags$hr(),
                         helpText("Download Association Results:"),
                         downloadButton("download.all.multi", "Download", class = "butt"),
                         tags$hr(),
                         helpText("Download Preprocessed Data:"),
                         downloadButton("download.dat", "Download", class = "butt"),
                         tags$hr(),
                         helpText("Download Metabolite Annotation:"),
                         downloadButton("download.annot.m", "Download", class = "butt"),
                         tags$hr(),
                         helpText("Download Covariate Annotation:"),
                         downloadButton("download.annot.c", "Download", class = "butt")
                       ) # conditionalPanel
                     ), # sidebarPanel
                     mainPanel(
                       h3("Covariate Selection"),
                       tags$hr(),
                       verbatimTextOutput("covar.select.start"),
                       verbatimTextOutput("covar.select.stop"),
                       tabsetPanel(id = "ChangeBasedOnThis", 
                                   tabPanel("Description", 
                                            h4("Methods Description"),
                                            tags$hr(),
                                            textOutput("selection.description")),
                                   tabPanel(title = "Plot",
                                            h4("Covariate Selection"),
                                            tags$hr(),
                                            plotOutput("multi.plot", hover = T, width = "70%")
                                   ),
                                   tabPanel("Relevant Covariates",
                                            h4("Your relevant covariates are:"),
                                            helpText("This table displays the maximum-partial-r-squared found per covariate per cohort that passed the set threshold."),
                                            tags$hr(),
                                            dataTableOutput("res.full.model")
                                   ),
                                   tabPanel("Association Statistics", 
                                            h4("Association Statistics"),
                                            tags$hr(),
                                            dataTableOutput("res.all.multi")),
                                   tabPanel("Metabolite Annotation",
                                            h4("Your Metabolite Annotation"),
                                            tags$hr(),
                                            dataTableOutput("res.annot.m")),
                                   tabPanel("Covariate Annotation",
                                            h4("Your Covariate Annotation"),
                                            tags$hr(),
                                            dataTableOutput("res.annot.c"))
                       )
                     ) # mainPanel
                   ) # sidebarLayout
                 })
)