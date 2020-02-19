
<!-- README.md is generated from README.Rmd. Please edit that file -->

# affiliationExplorer

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of affiliationExplorer is to provide a user-friendly graphical
interface to choose between different conflicting affiliations.

You need to provide to upload two files to the application:

  - a `biom` file
  - a multihits file (typically called `multihits_blabla.tsv`)

The app will then sort the multi-affiliated taxa from most abundant to
least abundant and help you pick one (or none) of the conflicting
affiliations. You can also edit the affiliation manually.

Finally, use the `Download` button to download the biom file with
corrected affiliations. This cleaned biom can be parsed with
`import_frogs()` from the `phyloseq.extended`
[package](https://github.com/mahendra-mariadassou/phyloseq-extended/tree/dev)

**Note** `affiliationExplorer` uses short taxa names, which may differ
from the original names, in the display. However, names from the
original biom file are preserved in the cleaned biom.

## Installation

You can install the latest version of `affiliationExplorer` from
[github](https://github.com/mahendra-mariadassou/affiliationExplorer)
with:

``` r
## install.packages("remotes")
remotes::install_github("mahendra-mariadassou/affiliationExplorer")
```

## Launching the app

You can launch on your computer as follows:

``` r
library(affiliationExplorer)
affiliationExplorer::run_app()
```
