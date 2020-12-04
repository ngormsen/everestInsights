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
    box(width = 12,
        img(src = "www/report_churn_1.png", width = "100%")
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
 
