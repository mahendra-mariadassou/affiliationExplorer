## Template for reading biom file 
read_frogs_biom <- function(biom_file) {
  phyloseq.extended::import_frogs(biom_file)
}

## Template for reading tsv files
read_multihits <- function(multihits_file) {
  readr::read_tsv(multihits_file)
}

## Functions to create short taxa names -------------------------------------
long_taxa_names <- function(taxa_names, max_size = 32) {
  ## any taxa name longer than 32 characters
  any(taxa_names >= max_size)
}

ambiguous_taxa <- function(physeq) {
  phyloseq::tax_table(physeq) %>% as("matrix") %>% 
    apply(MARGIN = 1, FUN = function(x) { any(x == "Multi-affiliation")}) %>% 
    which() %>% names()
}

create_otu_dictionary <- function(physeq) {
  otu_dictionary <- tibble::tibble(
    sequence  = phyloseq::taxa_names(physeq), 
    abundance = phyloseq::taxa_sums(physeq)
  ) %>% 
    dplyr::arrange(desc(abundance))
  ## If all taxa names are long, sequences are likely to have been processed by DADA
  ## Move OTU names to sequence column and rename OTU with shorter names
  if (long_taxa_names(otu_dictionary$sequence)) {
   otu_dictionary <- otu_dictionary %>% dplyr::mutate(OTU = paste0("ASV", 1:n()))
  } else {
    otu_dictionary <- otu_dictionary %>% dplyr::mutate(OTU = sequence)
  }
  otu_dictionary
}

sanitize_physeq_and_affi <- function(physeq, affi) {
  ## Create dictionary
  otu_dictionary <- create_otu_dictionary(physeq)
  old_to_new <- otu_dictionary %>% dplyr::select(-abundance) %>% tibble::deframe()
  
  ## Add abundance information to affiliation table
  affi <- inner_join(otu_dictionary, affi, by = c("sequence" = "#observation_name")) %>% 
    tidyr::separate(blast_taxonomy, into = phyloseq::rank_names(physeq), sep = ";")
  
  ## Rename taxa in physeq object
  phyloseq::taxa_names(physeq) <- old_to_new[phyloseq::taxa_names(physeq)] %>% unname()
  
  ## Remove previously curated taxa from affi
  affi <- affi %>% filter(OTU %in% ambiguous_taxa(physeq))
  
  list(
    physeq         = physeq, 
    affi           = affi, 
    otu_dictionary = otu_dictionary
  )
}

## Order OTU by priority level
sort_ambiguous_otu <- function(physeq, affi) {
  affi %>% dplyr::arrange(desc(abundance)) %>% dplyr::pull(OTU)
}

## Extract all affiliation for a given OTU
extract_affiliation <- function(affi, otu) {
  affi %>% 
    dplyr::filter(OTU == otu) %>% 
    dplyr::select(Kingdom:Species)
}

## Find level of ambiguity
find_level <- function(data) {
  not_consistent <- function(x) { !all(x == x[1]) }
  rank_names <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
  for (rank in rank_names) {
    rank_content <- data[[rank]]
    if (not_consistent(rank_content)) return(rank)
  }
}
