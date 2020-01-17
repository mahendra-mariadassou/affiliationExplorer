## code to prepare `test_data` dataset goes here
library(dplyr)
library(phyloseq)
physeq <- phyloseq.extended::import_frogs("data-raw/biom_16S_1M.biom")
affi <- readr::read_tsv("data-raw/multihits_16S_1M.tsv")

## Sanitize taxa names (change long sequences to short ones)
asv_dictionary <- tibble(sequence  = taxa_names(physeq), 
                         abundance = taxa_sums(physeq)) %>% 
  arrange(desc(abundance)) %>% 
  mutate(ASV = paste0("ASV", 1:n()))

affi <- inner_join(asv_dictionary, affi, by = c("sequence" = "#observation_name")) %>% 
  tidyr::separate(blast_taxonomy, into = rank_names(physeq), sep = ";")
sequence_to_asv <- asv_dictionary %>% select(-abundance) %>% tibble::deframe()
taxa_names(physeq) <- sequence_to_asv[taxa_names(physeq)] %>% unname()

## Save example biom and example multihit table
usethis::use_data(physeq)
usethis::use_data(affi)



## Prioritize orphan taxa


usethis::use_data("test_data")
