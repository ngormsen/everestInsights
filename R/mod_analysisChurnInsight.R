#' analysisChurnDashboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' 
#' @importFrom shiny NS tagList 
mod_analysisChurnInsight_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    box( width = 12,
        img(src = "www/churn_insights.png", width = "100%")
    )
  )
  
}
    
#' analysisChurnDashboard Server Function
#'
#' @noRd 
mod_analysisChurnInsight_server <- function(input, output, session, report){
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
# mod_analysisChurnInsight_ui("analysisChurnDashboard_ui_1")
    
## To be copied in the server
# callModule(mod_analysisChurnInsight_server, "analysisChurnDashboard_ui_1")
 
