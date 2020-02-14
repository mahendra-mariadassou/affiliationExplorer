#' @import shiny
#' @importFrom DT renderDT
#' @importFrom phyloseq tax_table
#' @importFrom phyloseq.extended write_phyloseq
#' @import shinyjs
app_server <- function(input, output, session) {
  # Load package data in the session (for testing purpose)
  # data("physeq", package = "affiliationExplorer")
  # data("affi", package = "affiliationExplorer")
  # data("otu_dictionary", package = "affiliationExplorer")
  # List the first level callModules here
  
  shinyjs::hide("clean")
  shinyjs::hide("download")
  output$tmptxt <- renderUI("Please upload your data (Biom file and MultiHits TSV file).")
  
  observeEvent(input$tsv, {
    
    shinyjs::show("clean")
    shinyjs::show("download")
    
    output$tmptxt <- renderUI("")
    
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
    ambiguous_otu <- unique(affi$OTU)
    ## Store cleaned otu as reactive values
    otu <- reactiveValues(
      cleaned = ambiguous_otu
    )
    
    # Add ASV Select Input
    insertUI(
      selector = "#tmp",
      where = "beforeEnd",
      ui = selectInput("asv",
                       label = "Select ASV",
                       choices = ambiguous_otu,
                       multiple = FALSE)
    )

    old_affiliations <- phyloseq::tax_table(physeq)[ambiguous_otu, ] %>% as("matrix")
    ## Store cleaned affiliations as reactive values    
    affiliations <- reactiveValues(
      cleaned = old_affiliations
    )

    observeEvent(input$asv, {
      # Extract Affiliation for a given OTU
      data <- extract_affiliation(affi, input$asv)
      amb <- find_level(data)
      output$txt <- renderUI({paste(input$asv, "- ", nrow(data) ," affiliation, ambiguity at rank ", amb)})
      output$table <- DT::renderDT({data}, selection = 'single')
      
      ## Show considered replacement if one is selected
      output$selection <- renderUI({
        s = input$table_rows_selected
        if (length(s)) {
          paste(
            "Current affiliation:",
            paste(old_affiliations[input$asv, ], collapse = ';'), 
            "to be replaced with:", 
            paste(data[s, ], collapse = ';'),
            sep = "\n") 
        }
      })
      
      ## Replace affiliation upon confirmation
      observeEvent(input$clean, {
        
        s = input$table_rows_selected
        if (length(s)) {
          ## Update affiliations
          affiliations$cleaned[input$asv, ] <- unlist(data[s, ])
          # Update otu
          otu$cleaned <- setdiff(otu$cleaned, input$asv)
          
          updateSelectInput(session, "asv",
                            label =  "Select ASV",
                            choices = otu$cleaned,
                            selected = tail(otu$cleaned, 1)
          )
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
        ## Update taxonomy of object phyloseq
        phyloseq::tax_table(physeq)[rownames(affiliations$cleaned), ] <- affiliations$cleaned
        ## revert short OTU names back to original names
        dict <- setNames(object = otu_dictionary$sequence, 
                         nm     = otu_dictionary$OTU)
        phyloseq::taxa_names(physeq) <- dict[phyloseq::taxa_names(physeq)]
        ## Export 
        phyloseq.extended::write_phyloseq(
          physeq = physeq, 
          biom_file = con, 
          biom_format = "frogs"
        )
      })
  })
}