## code to prepare `test_data` dataset goes here
library(dplyr)
library(phyloseq)
source("R/utils.R")
physeq <- read_frogs_biom("data-raw/biom_16S_1M.biom")
affi <- read_multihits("data-raw/multihits_16S_1M.tsv")

## Sanitize taxa names (change long sequences to short ones)
cleaned_data <- sanitize_physeq_and_affi(physeq, affi)
physeq           <- cleaned_data$physeq
affi             <- cleaned_data$affi
otu_dictionary   <- cleaned_data$otu_dictionary

## Save example biom and example multihit table
usethis::use_data(physeq,         overwrite = TRUE)
usethis::use_data(affi,           overwrite = TRUE)
usethis::use_data(otu_dictionary, overwrite = TRUE)
