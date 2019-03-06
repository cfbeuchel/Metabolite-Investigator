# Metabolite-Investigator

## Quick-Start

This is a Shiny-App. Proper documentation will be added soon. For now, the app
can be started directly from this repository via:

```r
# load the Shiny package # to install shiny run: install.packages("shiny")
library("shiny")

# Start the App directly through Github runGitHub("Metabolite-Investigator",
"cfbeuchel")
```

The App comes with example data from two cohorts that may be used to try out
the functionality. However, the data is currently not available online. Until
then, the `Use Example Data` button will remain without function. 

## Requirements

The app requires an up-to-date R installation as well as the following
packages:

* data.table (install via CRAN - `install.packages("data.table")`)
* sva (install via
  [Bioconductor](https://bioconductor.org/packages/release/bioc/html/sva.html))
* corrplot (install via CRAN - `install.packages("corrplot")`)
* ggplot2 (install via CRAN - `install.packages("ggplot2")`)
* scales (install via CRAN - `install.packages("scales")`)
