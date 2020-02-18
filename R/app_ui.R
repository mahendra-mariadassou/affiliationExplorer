#' @import shiny
#' @import shinycssloaders
#' @import shinydashboard
#' @import shinyjs
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    dashboardPage(skin = "purple",
      # DashBooard Header
      dashboardHeader(
        title = "Affiliation explorer"
      ),
      # DashBooard Sidebar
      dashboardSidebar(
        fileInput("biom",
                  "Upload Biom File",
                  accept = c("text/plain", ".biom")
                 ),
        fileInput("tsv",
                  "Upload MultiHits TSV File",
                  accept = c("text/tab-separated-values", ".tsv", "text/csv")
                 ),
        textOutput("tmp")
      ),
      # DashBooard Body
      dashboardBody(
        fluidPage(
          useShinyjs(),
          fluidRow(
            box(title = "Affiliation selection",
                width = NULL,
                status = "primary",
                solidHeader = T,
                htmlOutput("tmptxt"),
                htmlOutput("txt"),
                HTML("<br/>"),
                DT::DTOutput("table"),
                #HTML("<br/>"),
                htmlOutput("selection"),
                HTML("<br/>"),
                actionButton("clean", "Clean ASV"),
                actionButton("skip", "Skip ASV"),
                downloadButton("download", "Download")
            )
          )
        )
      )
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  
  addResourcePath(
    'www', system.file('app/www', package = 'affiliationExplorer')
  )
 
  tags$head(
    golem::activate_js(),
    golem::favicon()
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
