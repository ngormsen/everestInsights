#' ReportCard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ReportCard_ui <- function(id){
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
                 textOutput(ns("text"))
               ),
               fluidRow(
                 column(4, actionButton(ns("dashboardButton"), "Add to Dashboard")),
                 column(3, actionButton(ns("reportButton"), "Open Report"))
               )
        ),
        column(4,shinycssloaders::withSpinner(plotOutput(ns("image"), height = "200px"), type = 7))
      ),
      width = 12,
      height = "250px"
    )
  )
}
    
#' ReportCard Server Function
#'
#' @noRd 
mod_ReportCard_server <- function(input, output, session, report, dashboardSession, reportIdx){
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
    updateTabItems(session = dashboardSession, "tabsMenu", paste0("report_", reportIdx))
  })
  
  
  
}
    
## To be copied in the UI
# mod_ReportCard_ui("ReportCard_ui_1")
    
## To be copied in the server
# callModule(mod_ReportCard_server, "ReportCard_ui_1")
 
