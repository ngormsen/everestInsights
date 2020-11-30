

#' reportView UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @import knitr
#' @importFrom shiny NS tagList 
mod_reportViewHolder_ui <- function(id, reportId){
  ns <- NS(id)
  
  tabItem(
    tabName = reportId,
    uiOutput(ns("insight")),
    uiOutput(ns("view"))
    
  )
}
    
#' reportView Server Function
#'
#' @noRd 
mod_reportViewHolder_server <- function(input, output, session, report){
  ns <- session$ns
  output$insight <- renderUI({
    report$getViewInsightServer()
    report$getViewInsightUi()
  })
  output$view <- renderUI({
    report$getViewServer()
    report$getViewUi()
  })
  
}
    
## To be copied in the UI
# mod_reportView_ui("reportView_ui_1")
    
## To be copied in the server
# callModule(mod_reportView_server, "reportView_ui_1")
 
