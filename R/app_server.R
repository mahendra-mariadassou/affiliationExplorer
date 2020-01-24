#' @import shiny
#' @importFrom DT DT
#' @importFrom phyloseq tax_table
app_server <- function(input, output, session) {
  # Load package data in the session (for testing purpose)
  # data("physeq", package = "affiliationExplorer")
  # data("affi", package = "affiliationExplorer")
  # data("otu_dictionary", package = "affiliationExplorer")
  # List the first level callModules here
  
  observeEvent(input$tsv, {
    # Read the biom file --> phyloseq
    biomfile <- read_frogs_biom(input$biom$datapath)
    # Read the tsv file --> readr
    multihits <- read_multihits(input$tsv$datapath)
    # Sanitize physeq, multi_hits and build dictionary for short OTU names
    all <- sanitize_physeq_and_affi(biomfile, multihits)
    physeq <- all$physeq
    dict   <- all$otu_dictionary
    affi   <- all$affi  
    # Ambiguous ASVs and their affiliation
    ambiguous_otu <- unique(affi$OTU) ## this probably needs to be reactive
    ambiguous_otu_affi <- phyloseq::tax_table(physeq)[ambiguous_otu, ] %>% as("matrix")
    # Add ASV Select Input
    insertUI(
      selector = "#tmp",
      where = "beforeEnd",
      ui = selectInput("asv",
                       label = "Select ASV",
                       choices = ambiguous_otu,
                       multiple = FALSE)
    )
    
    observeEvent(input$asv, {
      
      # Extract Affiliation for a given OTU
      data <- extract_affiliation(affi, input$asv)
      amb <- find_level(data)
      output$txt <- renderUI({paste(input$asv, "- ", nrow(data) ," affiliation, ambiguity at rank ", amb)})
      output$table <- DT::renderDT({data}, selection = 'single')
      
      # If a column is selected, update affiliation
      output$selection <- renderUI({
        s = input$table_rows_selected
        if (length(s)) {
          paste("These rows were selected: ", s)
        }
      })
    })
    
    ## Reactive block to update ambiguous_otu, ambiguous_otu_affi and the list when clicking on the "Clean ASV" button
    ## cleaned_taxonomy <- as.character(data[s, ])
    ## phyloseq::tax_table(physeq)[input$asv, ] <- cleaned_taxonomy
    ## ambiguous_otu <- setdiff(ambiguous_otu, input$asv)
    ## updateUI 
    
    output$download <- downloadHandler(
      filename = function() {
        paste0('cleaned_biom-', Sys.Date(), '.biom')
      },
      content = function(con) {
        phyloseq.extended::write_phyloseq(
          physeq = physeq, 
          biom_file = con, 
          biom_format = "frogs"
        )
        ## write.csv(data, con)
      })
  })
}