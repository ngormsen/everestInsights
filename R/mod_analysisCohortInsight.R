#' analysisCohortInsight UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_analysisCohortInsight_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    box(
      fluidRow(
        column(1),
        column(7,
               fluidRow(
                 textOutput(ns("title"))
               ),
               fluidRow(
                 textOutput(ns("text")),
                 textOutput(ns("randomText"))
               ),
        ),
        column(4,shinycssloaders::withSpinner(plotOutput(ns("image"), height = "200px"), type = 7))
      ),
      width = 12,
      height = "250px"
    )
  )
}
    
#' analysisCohortInsight Server Function
#'
#' @noRd 
mod_analysisCohortInsight_server <- function(input, output, session, report){
  ns <- session$ns
  reportData <- report$getReportData()
  
  output$title <- renderText({
    report$title
  })
  
  output$text <- renderText({
    report$text
  })
  
  output$image <- renderPlot({
    report$image
  })
  
  output$randomText <- renderText({
    if(!is.null(reportData$getRandomText())){
      reportData$getRandomText()
    }
    
  })
  
}
    
## To be copied in the UI
# mod_analysisCohortInsight_ui("analysisCohortInsight_ui_1")
    
## To be copied in the server
# callModule(mod_analysisCohortInsight_server, "analysisCohortInsight_ui_1")
 
