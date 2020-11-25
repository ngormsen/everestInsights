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
        column(4,plotOutput(ns("image"), height = "200px"))
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
  output$image <- renderPlot({
    report$image
  })
  output$text <- renderText({
    report$text
  })
  output$title <- renderText({
    report$title
  })
  
  observeEvent(input$dashboardButton, {
    report$activateDashboard()
  })
  
  observeEvent(input$reportButton, {
    updateTabItems(session = dashboardSession, "tabsMenu", paste0("report_", reportIdx))
  })
  
  
  
}
    
## To be copied in the UI
# mod_ReportCard_ui("ReportCard_ui_1")
    
## To be copied in the server
# callModule(mod_ReportCard_server, "ReportCard_ui_1")
 
