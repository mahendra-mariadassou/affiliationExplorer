#' @import shiny
#' @importFrom DT renderDT editData
#' @importFrom phyloseq tax_table taxa_names taxa_sums
#' @importFrom phyloseq.extended write_phyloseq
#' @importFrom shinyjs hide show
#' @importFrom dplyr distinct
app_server <- function(input, output, session) {
  ## Change maximum upload size
  max_size <- golem::get_golem_options("max_size")
  if (!is.null(max_size)) options(shiny.maxRequestSize = max_size)
  
  # Load package data in the session (for testing purpose)
  # data("physeq", package = "affiliationExplorer")
  # data("affi", package = "affiliationExplorer")
  # data("otu_dictionary", package = "affiliationExplorer")
  # List the first level callModules here
  
  shinyjs::hide("asv")
  shinyjs::hide("clean")
  shinyjs::hide("skip")
  shinyjs::hide("download")
  
  output$tmptxt <- renderUI(HTML("<p>Please upload your data (Biom file and MultiHits TSV file).</p>"))
  output$tmptxt2 <- renderUI(HTML("<p>Please upload your data (Biom file and MultiHits TSV file).</p><br/><br/>"))
  
  observeEvent(input$tsv, {
    
    shinyjs::show("clean")
    shinyjs::show("skip")
    shinyjs::show("download")
    
    output$tmptxt <- renderUI("")
    output$tmptxt2 <- renderUI("")
    
    # Read the biom file --> phyloseq
    biomfile <- read_frogs_biom(input$biom$datapath)
    # Read the optional fasta file --> read_fasta
    fasta <- read_fasta(input$fasta$datapath)
    # Read the tsv file --> readr
    multihits <- read_multihits(input$tsv$datapath)
    # Sanitize physeq, multi_hits and build dictionary for short OTU names
    all <- sanitize_physeq_and_affi(biomfile, multihits, fasta)
    physeq <- all$physeq
    dict   <- all$otu_dictionary
    affi   <- all$affi  
    # Store ambiguous ASVs and their affiliation in reactive environment
    data <- reactiveValues(
      amb_otus     = unique(affi$OTU),                             ## Ambiguous otus
      cleaned      = phyloseq::tax_table(physeq) %>% as("matrix"), ## All current affiliations (not only those of ambiguous OTUs)
      affi         = NULL,                                         ## Placeholder for conflicting affiliations of current OTU
      sequence     = NULL                                          ## Placeholder for current OTU sequence
    )
    ## Sort `cleaned` by decreasing taxa abundances
    data$cleaned <- data$cleaned[phyloseq::taxa_sums(physeq) %>% sort(decreasing = TRUE) %>% names(), ]
    
    updateSelectInput(session, "asv",
                      label =  "Select ASV",
                      choices = data$amb_otus
    )
    
    shinyjs::show("asv")
    
    # Add Sequence Checkbox
    insertUI(
      select = "#table",
      where = "afterEnd",
      ui = checkboxInput("seq",
                         label = "Show sequence",
                         value = FALSE
                         )
    )

    ### Page 1 UI elements -----------------------------------------------
    
    observeEvent(input$asv, {
      # Extract Affiliation for a given OTU
      data$affi <- extract_affiliation(affi, input$asv)
      data$sequence <- extract_sequence(affi, input$asv)
      amb <- find_level(data$affi)
      output$txt <- renderUI(HTML({paste("<p><b>", input$asv, "- ", nrow(data$affi) ,"conflicting affiliations, ambiguity at rank ", amb, "</b></p>")}))
      
      output$help <- renderUI(HTML({paste("<cite>Select new affiliation by clicking on a row (double click on a cell to edit its content).<br/>",
                                          "Click \"Update ASV\" to update affiliation (with selected row) or \"Skip ASV\" to move to the next one.</cite>")}))
      
      output$table <- DT::renderDT({data$affi}, 
                                   selection = list(mode = 'single', selected = NULL, target = 'row'), 
                                   editable = TRUE)
        
      ## Show considered replacement if one is selected
      output$selection <- renderUI({
        s = input$table_rows_selected
        if (length(s)) {
          
          HTML(paste("<b>Current affiliation:</b><br/>&nbsp;&nbsp;&nbsp;"),
               paste(data$cleaned[input$asv, ], collapse = ' / '),
               "<br/><b>to be replaced with:</b><br/>&nbsp;&nbsp;&nbsp;",
               paste(data$affi[s, ], collapse = ' / ')
          )
        }
      })
    })
    
    observeEvent(input$seq, {
      if (input$seq) {
        output$sequence <- renderUI(HTML({paste("<b>Sequence:</b><br/>", paste(unlist(strsplit(gsub("(.{80})", "\\1 ", data$sequence), " ")), collapse = "<br/>"), "<br/><br/>")}))
      }
      else {
        output$sequence <- renderUI(HTML(""))
      }
    })
    
    ## Allow manual corrections
    observeEvent(input$table_cell_edit, {
      data$affi[,] <<- DT::editData(data$affi, input$table_cell_edit, "table")
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
      } else {
        output$selection <- renderUI({
          HTML(paste("Choose an affiliation before clicking on the \"Update ASV\" button"))
        })
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
    
    ## Download biomfile
    output$download <- downloadHandler(
      filename = function() {
        paste0('cleaned_biom-', Sys.Date(), '.biom')
      },
      content = function(con) {
        ## Update taxonomy of object phyloseq
        phyloseq::tax_table(physeq)[ , ] <- data$cleaned[phyloseq::taxa_names(physeq), ]
        ## revert short OTU names back to original names
        dict <- setNames(object = dict$sequence, 
                         nm     = dict$OTU)
        phyloseq::taxa_names(physeq) <- dict[phyloseq::taxa_names(physeq)] %>% unname()
        ## Export 
        phyloseq.extended::write_phyloseq(
          physeq = physeq, 
          biom_file = con, 
          biom_format = "frogs"
        )
      })
    
    ### Page 2 UI elements -----------------------------------------------
    output$tableFull <- DT::renderDT({data$cleaned}, 
                                     filter = "top",
                                     selection = list(mode = 'single', selected = NULL, target = 'row'), 
                                     editable = TRUE)
    
    ## Manual corrections in non ambiguous taxa
    observeEvent(input$tableFull_cell_edit, {
      data$cleaned[,] <<- DT::editData(data$cleaned, input$tableFull_cell_edit, "tableFull")
    })
    
  })
}