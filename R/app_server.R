#' @import shiny
#' @importFrom DT renderDT editData
#' @importFrom phyloseq tax_table
#' @importFrom phyloseq.extended write_phyloseq
#' @importFrom shinyjs hide show
#' @importFrom dplyr distinct
app_server <- function(input, output, session) {
  # Load package data in the session (for testing purpose)
  # data("physeq", package = "affiliationExplorer")
  # data("affi", package = "affiliationExplorer")
  # data("otu_dictionary", package = "affiliationExplorer")
  # List the first level callModules here
  
  shinyjs::hide("clean")
  shinyjs::hide("skip")
  shinyjs::hide("download")
  output$tmptxt <- renderUI("Please upload your data (Biom file and MultiHits TSV file).")
  
  observeEvent(input$tsv, {
    
    shinyjs::show("clean")
    shinyjs::show("skip")
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
    # Store ambiguous ASVs and their affiliation in reactive environment
    data <- reactiveValues(
      amb_otus = unique(affi$OTU),                                                 ## Ambiguous otus
      cleaned  = phyloseq::tax_table(physeq)[unique(affi$OTU), ] %>% as("matrix"), ## Their current affiliation
      affi     = NULL                                                              ## Placeholder for conflicting affiliations of current ASV
    )
    
    # Add ASV Select Input
    insertUI(
      selector = "#tmp",
      where = "beforeEnd",
      ui = selectInput("asv",
                       label = "Select ASV",
                       choices = data$amb_otus,
                       multiple = FALSE)
    )

    observeEvent(input$asv, {
      # Extract Affiliation for a given OTU
      data$affi <- extract_affiliation(affi, input$asv) %>% dplyr::distinct()
      amb <- find_level(data$affi)
      output$txt <- renderUI({paste(input$asv, "- ", nrow(data$affi) ,"conflicting affiliation, ambiguity at rank ", amb)})
      output$table <- DT::renderDT({data$affi}, 
                                   selection = list(mode = 'single', selected = NULL, target = 'row'), 
                                   editable = TRUE)
      ## Show considered replacement if one is selected
      output$selection <- renderUI({
        s = input$table_rows_selected
        if (length(s)) {
          paste(
            "Current affiliation:",
            paste(data$cleaned[input$asv, ], collapse = ';'), 
            "to be replaced with:", 
            paste(data$affi[s, ], collapse = ';'),
            sep = "\n") 
        }
      })
    })
    
    ## Allow manual corrections
    observeEvent(input$table_cell_edit, {
      data$affi <<- DT::editData(data$affi, input$table_cell_edit, "table")
    })
    
    ## Replace affiliation upon confirmation
    observeEvent(input$clean, {
      s = input$table_rows_selected
      if (length(s)) {
        ## Update affiliations
        data$cleaned[input$asv, ] <- unlist(data$affi[s, ])
        data$amb_otus <- setdiff(data$amb_otus, input$asv)
        updateSelectInput(session, "asv",
                          label =  "Select ASV",
                          choices = data$amb_otus,
                          selected = data$amb_otus[1]
        )
      }        
    }
    )
    
    ## Skip ASV
    observeEvent(input$skip, {
      
      data$amb_otus <- setdiff(data$amb_otus, input$asv)
      
      updateSelectInput(session, "asv",
                        label =  "Select ASV",
                        choices = data$amb_otus,
                        selected = data$amb_otus[1]
      )
    }
    )
    
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