#' @import shiny
#' @import shinycssloaders
#' @import shinydashboard
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
                  accept = c(".biom")
                 ),
        fileInput("tsv",
                  "Upload MultiHits TSV File",
                  accept = c("text/csv", ".tsv")
                 ),
        textOutput("tmp")
      ),
      # DashBooard Body
      dashboardBody(
        fluidPage(
          fluidRow(
            box(title = "Affiliation selection",
                width = NULL,
                status = "primary",
                solidHeader = T,
                htmlOutput("txt"),
                HTML("<br/>"),
                tableOutput("table")
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
