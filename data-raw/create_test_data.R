## code to prepare `test_data` dataset goes here
library(dplyr)
library(phyloseq)
physeq <- phyloseq.extended::import_frogs("data-raw/biom_16S_1M.biom")
affi <- affi <- readr::read_tsv("data-raw/multihits_16S_1M.tsv")

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

## Find level of ambiguity
find_level <- function(Kingdom, Phylum, Class, Order, Family, Genus, Species) {
  not_consistent <- function(x) { !all(x == x[1]) }
  rank_names <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
  for (rank in rank_names) {
    rank_content <- get(rank)
    if (not_consistent(rank_content)) return(rank)
  }
}


## Prioritize orphan taxa


usethis::use_data("test_data")
