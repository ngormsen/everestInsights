#' tabDashboardReport UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @import shinipsum
#' @importFrom shiny NS tagList observeEvent
mod_tabDashboardReport_ui <- function(id){
  ns <- NS(id)
  
  
  tabItem(
    tabName = "tabReports",
    uiOutput(ns("content")),
    actionButton(ns("do"), "Click Me")
  )
}
    
#' tabDashboardReport Server Function
#'
#' @noRd 
mod_tabDashboardReport_server <- function(input, output, session, dashboardSession, reports){
  ns <- session$ns
  
  reportsUI <- list()
  
  for(i in 1:length(reports)){
    reportsUI[[i]] <- mod_ReportCard_ui(ns(paste0("ReportCard_ui_", i)))
  }
  
  lapply(seq_along(reports),
         function(i){
           callModule(
             mod_ReportCard_server, 
             paste0("ReportCard_ui_", i), 
             reports[[i]],
             dashboardSession,
             i
           )
         })

  output$content <- renderUI({
    reportsUI
  }
  
  
  )
  
  
}
    
## To be copied in the UI
# mod_tabDashboardReport_ui("tabDashboardReport_ui_1")
    
## To be copied in the server
# callModule(mod_tabDashboardReport_server, "tabDashboardReport_ui_1")
 
