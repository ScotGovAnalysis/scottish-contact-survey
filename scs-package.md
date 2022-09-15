
<!-- scs-package.md is generated from scs-package.Rmd. Please edit that file -->

# scs

[![GitHub release (latest by
date)](https://img.shields.io/github/v/release/DataScienceScotland/scottish-contact-survey)](https://github.com/DataScienceScotland/scottish-contact-survey/releases/latest)
[![R build
status](https://github.com/DataScienceScotland/scottish-contact-survey/workflows/R-CMD-check/badge.svg)](https://github.com/DataScienceScotland/scottish-contact-survey/actions)

This repository contains a [Reproducible Analytial Pipeline
(RAP)](https://gss.civilservice.gov.uk/reproducible-analytical-pipelines/)
for the Scottish Government Covid-19 [Scottish Contact Survey
(SCS)](README.md). The pipeline is written in R and takes the form of an
R package, called `scs`, and accompanying R scripts.

Currently, the RAP is used for a variety of data preparation steps
including; cleaning of the raw data as downloaded from Questback,
updates to participant data to reflect latest survey response
(e.g. household member changes) and production of invitee data file for
next wave of survey.

## Package Installation

To install `scs`, the package `remotes` is required. The package can be
installed directly from GitHub with:

``` r
remotes::install_github(
  "DataScienceScotland/scottish-contact-survey",
  upgrade = "never"
)
```

Network security settings may prevent `remotes::install_github()` from
working. If this is the case, `scs` can be installed by downloading the
[zip of the
repository](https://github.com/DataScienceScotland/scottish-contact-survey/archive/main.zip)
and running the following code (replacing the section marked `<>`,
including the arrows themselves):

``` r
remotes::install_local(
  "<FILEPATH OF ZIPPED FILE>/scottish-contact-survey-main.zip",
  upgrade = "never"
)
```

The `scs` package can then be loaded using the `library()` function:

``` r
library(scs)
```

To access the help file associated with a function within the `scs`
package, type `?function_name` into the RStudio console, for example:

``` r
?age
```
