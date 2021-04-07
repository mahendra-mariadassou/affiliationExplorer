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
        fluidPage(
          fileInput("biom",
                    "Upload Biom File",
                    accept = c(".biom", ".biom1")
                   ),
          fileInput("fasta",
                    "Optional: upload Fasta File",
                    accept = c("text/plain", ".fasta", ".fst")
          ),
          fileInput("tsv",
                    "Upload MultiHits TSV File",
                    accept = c("text/tab-separated-values", ".tsv", ".tabular")
                   ),
          HTML("<br/>"),
          actionButton("upload", "Submit data", class = "butt", icon = icon("cloud-upload")),
          downloadButton("download", "Download data", class="butt"),
          tags$head(tags$style(".butt{color: black !important; margin-left: 16px; }"))
        )
      ),
      # DashBooard Body
      dashboardBody(
        tags$style(HTML("
          .tabbable > .nav > li > a               { background-color: #3c8dbc; color:white; font-size: 17px; }
          .tabbable > .nav > li[class=active] > a { background-color: white; color:#3c8dbc; font-size: 17px; }
          ")),
        tabsetPanel(
          tabPanel("Affiliation selection",
            useShinyjs(),
            fluidRow(
              box(
                width = 12,
                solidHeader = T,
                htmlOutput("tmptxt"),
                div(style="display: inline-block; width: 100px;",
                    selectInput("asv", label = "Select OTU", choices = c(), multiple = FALSE)),
                div(style="display: inline-block; margin-left: 75%;",
                    actionButton("clean", "Update OTU", class="butt2")),
                div(style="display: inline-block;",
                    actionButton("skip", "Skip OTU", class="butt3")),
                tags$head(tags$style(".butt2{margin-bottom: 28px; align: right;}")),
                tags$head(tags$style(".butt3{margin-bottom: 28px; align: right;}")),
                htmlOutput("txt"),
                htmlOutput("help"),
                HTML("<br/>"),
                DT::DTOutput("table"),
                htmlOutput("sequence"),
                htmlOutput("selection")
              )
            )
          ),
          tabPanel("Affiliation edition", 
            fluidRow(
              box(
                width = 12,
                solidHeader = T,
                htmlOutput("tmptxt2"),
                DT::DTOutput("tableFull")
              )
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
