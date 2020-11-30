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
 