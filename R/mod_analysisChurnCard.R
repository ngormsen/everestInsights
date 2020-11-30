#' analysisChurnCard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_analysisChurnCard_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    column(1),
    column(7,
           fluidRow(
             textOutput(ns("title"))
           ),
           fluidRow(
             textOutput(ns("text"))
           )
    ),
    column(4,shinycssloaders::withSpinner(plotOutput(ns("image"), height = "200px"), type = 7))
  )}
    
#' analysisChurnCard Server Function
#'
#' @noRd 
mod_analysisChurnCard_server <- function(input, output, session, report){
  ns <- session$ns
  
  output$title <- renderText({
    report$title
  })
  
  output$text <- renderText({
    report$text
  })
  
  output$image <- renderPlot({
    report$image
  })
  
  observeEvent(input$dashboardButton, {
    if(report$getDashboard() == FALSE){
      report$activateDashboard()
      updateActionButton(session, "dashboardButton", label = "Remove from Dashboard")
    }
    else{
      report$deactivateDashboard()
      updateActionButton(session, "dashboardButton", label = "Add to Dashboard")
    }
  })
  
  observeEvent(input$reportButton, {
    updateTabItems(session = dashboardSession, "tabsMenu", report$getId())
  })
  
}
    
## To be copied in the UI
# mod_analysisChurnCard_ui("analysisChurnCard_ui_1")
    
## To be copied in the server
# callModule(mod_analysisChurnCard_server, "analysisChurnCard_ui_1")
 
