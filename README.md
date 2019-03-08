# Metabolite-Investigator

## Quick-Start

This is a Shiny-App. Proper documentation will be added soon. For now, the app
can be started directly from this repository via:

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
