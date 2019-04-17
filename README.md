# Metabolite-Investigator

## Quick-Start

This is a Shiny-App with the aim of facilitating association and covariate selection for targeted metabolomics data from multiple studies with a wide range of factors (demographic, lifestyle, etc.). Features of the App are:

* Upload of data (with seperate files for metabolite and factor data)
* Matching via ID overlap
* Preprocessing of data in each cohort including
  + Removal of outliers defined as values +5 times the sample standard deviation (5*SD) on log-transformed data
  + Inverse-normal-transformation of data to remove skew while regainig data structure (concerning zero-inflated values)
  + Nonparametric batch-adjustment via an empirical Bayes method ([sva::ComBat](https://bioconductor.org/packages/release/bioc/html/sva.html))
* Univariable association step of each metabolite with each factor in each cohort (Metabolite_i ~ Factor_j)
* Correlation check and user guided descision on exclusion of too highly correlating factors
* Multivariable association step of each metabolite with all factors in each cohort (Metabolite_i ~ Factor_1 + Factor_2 + ... Factor_J)
* Selection of covariates for subsequent analyses of metabolites by removing factors not explaining a set amount of variance in at least one metabolite in at least one cohort via backwards selection until only factors meeting the explained variance criteria remain in the model 
* Visualization of results
* Sample and feature-centric annotation
* Download of results

A detailed description of the methods implemented and an application with data for 63 metabolites and 29 factors in three studies (N=16,222) can **soon** be found [here]()

The app can be started directly from this repository via:

```r
# to install shiny run:
# install.packages("shiny")
# load the Shiny package
library("shiny")

# Start the App directly through Github
runGitHub("Metabolite-Investigator", "cfbeuchel")
```

The App comes with example data from two cohorts that may be used to try out
the functionality. Press the `Use Example Data` button to load the data and try out the application. 

## Requirements

The app requires an up-to-date R installation as well as the following
packages:

* sva (install via
  [Bioconductor](https://bioconductor.org/packages/release/bioc/html/sva.html))
* data.table (install via CRAN - `install.packages("data.table")`)
* visNetwork (install via CRAN - `install.packages("visNetwork")`)
* magrittr (install via CRAN - `install.packages("magrittr")`)
* ggplot2 (install via CRAN - `install.packages("ggplot2")`)
* scales (install via CRAN - `install.packages("scales")`)
