## Template for reading biom file 
read_frogs_biom <- function(biom_file) {
  phyloseq.extended::import_frogs(biom_file)
}

## Template for reading tsv files
read_multihits <- function(multihits_file) {
  readr::read_tsv(multihits_file)
}

## Template for reading fasta files into a tibble
#' @importFrom Biostrings readDNAStringSet
read_fasta <- function(fasta_file) {
  if (is.null(fasta_file)) return(NULL)
  Biostrings::readDNAStringSet(fasta_file) %>% 
    as.character() %>% 
    tibble::tibble(OTU      = names(.), 
                   sequence = unname(.)) %>% 
    dplyr::select(OTU, sequence) %>% 
    dplyr::mutate(OTU = OTU %>% strsplit(split = " ", fixed = TRUE) %>% sapply(`[`, 1))
}

## Functions to create short taxa names -------------------------------------
long_taxa_names <- function(taxa_names, max_size = 32) {
  ## any taxa name longer than 32 characters
  any(nchar(taxa_names) >= max_size)
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
  ## If all taxa names are long, sequences are likely to have been processed by DADA2
  ## Move OTU names to sequence column and rename OTU with shorter names
  if (long_taxa_names(otu_dictionary$sequence)) {
   otu_dictionary <- otu_dictionary %>% 
     dplyr::mutate(OTU = paste0("ASV", 1:nrow(otu_dictionary)))
  } else {
    otu_dictionary <- otu_dictionary %>% dplyr::mutate(OTU = sequence)
  }
  otu_dictionary
}

sanitize_physeq_and_affi <- function(physeq, affi, fasta) {
  ## Create dictionary
  otu_dictionary <- create_otu_dictionary(physeq)
  old_to_new <- otu_dictionary %>% dplyr::select(-abundance) %>% tibble::deframe()
  
  ## Add abundance information to affiliation table
  affi <- dplyr::inner_join(otu_dictionary, affi, by = c("sequence" = "#observation_name")) %>% 
    tidyr::separate(blast_taxonomy, into = phyloseq::rank_names(physeq), sep = ";")
  
  ## Rename taxa in physeq object
  phyloseq::taxa_names(physeq) <- old_to_new[phyloseq::taxa_names(physeq)] %>% unname()
  
  ## Remove previously curated taxa from affi
  affi <- affi %>% dplyr::filter(OTU %in% ambiguous_taxa(physeq))
  
  ## Add sequences from fasta file to affiliation table
  if (!is.null(fasta)) {
    if (long_taxa_names(otu_dictionary$sequence)) { 
      warning("Sequences already present in the multihits files.\nFasta file not used.")
    } else {
      ## FROGS rather than DADA2, keep sequences from fasta file
      affi <- affi %>% dplyr::select(-sequence) %>% dplyr::left_join(fasta, by = "OTU")
    }
  }
  
  
  list(
    physeq         = physeq, 
    affi           = affi, 
    otu_dictionary = otu_dictionary
  )
}

## Other utilities ------------------------------------------

## Order OTU by priority level
sort_ambiguous_otu <- function(physeq, affi) {
  affi %>% dplyr::arrange(desc(abundance)) %>% dplyr::pull(OTU)
}

## Extract all affiliation for a given OTU
extract_affiliation <- function(affi, otu) {
  affi %>% 
    dplyr::filter(OTU == otu) %>% 
    dplyr::distinct(dplyr::across(Kingdom:Species), .keep_all = TRUE) %>% 
    dplyr::select(Kingdom:Species, blast_subject, blast_perc_identity, blast_perc_query_coverage) %>% 
    dplyr::rename(`Blast ID` = blast_subject, 
                  `%id`      = blast_perc_identity, 
                  `%cov`     = blast_perc_query_coverage)
}

remove_extra <- function(affi) {
  affi %>% dplyr::select(-any_of(c("Blast ID", "%id", "%cov")))
}

## Extract sequence for a given OTU
extract_sequence <- function(affi, otu) {
  affi %>% 
    dplyr::filter(OTU == otu) %>% 
    dplyr::pull(sequence) %>% 
    head(1)
}

## Find level of ambiguity
find_level <- function(data) {
  not_consistent <- function(x) { !all(x == x[1]) }
  rank_names <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
  for (rank in rank_names) {
    rank_content <- data[[rank]]
    if (not_consistent(rank_content)) return(rank)
  }
  warning("The provided taxa is not ambiguous")
  NULL
}

