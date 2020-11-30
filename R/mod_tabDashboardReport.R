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
    uiOutput(ns("content"))
  )
}
    
#' tabDashboardReport Server Function
#'
#' @noRd 
mod_tabDashboardReport_server <- function(input, output, session, dashboardSession, reports){
  ns <- session$ns
  
  reportsUI <- list()
  for(i in 1:length(reports)){
    
    reportsUI[[i]] <- fluidRow(
      box(
        reports[[i]]$getCardUi(),
        fluidRow(
          column(2, actionButton(ns("reportButton"), "Open Report")),
          column(4, actionButton(ns("dashboardButton"), "Add to Dashboard"))
        ),
        width = 12,
        height = "250px"
      )
    )
    
  }
  
  lapply(seq_along(reports),
         function(i){
           report <- reports[[i]]
           report$getCardServer()
           
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
         })

  output$content <- renderUI({
    reportsUI
  })
  
  
  
  
}
    
## To be copied in the UI
# mod_tabDashboardReport_ui("tabDashboardReport_ui_1")
    
## To be copied in the server
# callModule(mod_tabDashboardReport_server, "tabDashboardReport_ui_1")
 
