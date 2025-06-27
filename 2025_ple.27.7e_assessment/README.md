2025_ple.27.7e_assessment
================

## Plaice (*Pleuronectes platessa*) in Division 7.e (western English Channel) - WKBPLAICE 2025

This repository illustrates the stock assessment for plaice
(*Pleuronectes platessa*) in Division 7.e (western English Channel) in
`R` from WGCSE 2025.

The advice for this stock is based on the category 3 chr rule.

## R packages

The following R packages are needed:

``` r
icesTAF
icesAdvice
dplyr
tidyr
ggplot2
cat3advice
```

They can be installed with:

``` r
### install/update packages
install.packages(c("icesTAF", "icesAdvice", "dplyr", "tidyr", "ggplot2"))
### install cat3advice from ICES r-universe
# install.packages("cat3advice", repos = c("https://ices-tools-prod.r-universe.dev", "https://cloud.r-project.org"))
```

The `cat3advice` package is also needed and can be installd with

``` r
install.packages("cat3advice", repos = c("https://ices-tools-prod.r-universe.dev", "https://cran.r-project.org"))
### alternative installation from GitHub:
# remotes::install_github("shfischer/cat3advice")
```

For exact reproducibility, it is recommended to use exactly the same
package version as used in the assessment. These package are
automatically installed into a local library when running the TAF
assessment (see below) or by calling

``` r
library(icesTAF)
taf.boot()
```

## Running the assessment

The easiest way to run the assessment is to clone or download this
repository, navigate into the repository with R and run:

``` r
### load the icesTAF package
library(icesTAF)
### load data
taf.boot(software = FALSE)
### install R package from SOFTWARE.bib - only if needed
if (FALSE) taf.boot(software = TRUE, data = FALSE, clean = FALSE)
### run all scripts
source.all()
```

This code snippet runs the data compilation and assessment and creates
the tables and figures presented in the WG report.
