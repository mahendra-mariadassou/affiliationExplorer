#' @import shiny
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
    #
    all <- sanitize_physeq_and_affi(biomfile, multihits)
    # Add ASV Select Input
    insertUI(
      selector = "#tmp",
      where = "beforeEnd",
      ui = selectInput("asv",
      label = "Select ASV",
      choices = all$otu_dictionary$OTU,
      multiple = FALSE)
    )
    
    observeEvent(input$asv, {
      
      # Extract Affiliation for a given OTU
      data <- extract_affiliation(all$affi, input$asv)
      amb <- find_level(data)
      output$txt <- renderUI({paste(input$asv, "- ", nrow(data) ," affiliation, ambiguity at rank ", amb)})
      output$table <- DT::renderDT({data}, selection = 'single')
      
      output$selection <- renderUI({
        s = input$table_rows_selected
        if (length(s)) {
          paste("These rows were selected: ", s)
        }
      })
    })
    
    output$download <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(data, con)
      })
  })
}