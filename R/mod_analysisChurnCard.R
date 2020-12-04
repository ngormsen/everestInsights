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
  uiOutput(ns("somehing"))
  uiOutput(ns("htmlOutput"))
}

#' analysisChurnCard Server Function
#'
#' @noRd 
mod_analysisChurnCard_server <- function(input, output, session, report, dashboardSession){
  ns <- session$ns
  output$title <- renderUI({
    tags$h2(report$title)
  })
  
  output$text <- renderUI({
    paste0(report$text, sep = "\n")
  })
  
  output$image <- renderPlot({
    report$image
  })
  
  
  output$htmlOutput <- renderUI({
    fluidRow(
      box(
        fluidRow(
          column(8,
                 tags$div(
                   style = "height:170px",
                   uiOutput(ns("title")),
                   tags$br(),
                   uiOutput(ns("text")),
                   tags$br()
                 ),
                 tags$span(
                   actionButton(ns("reportButton"), "Open Report"),
                   actionButton(ns("dashboardButton"), "Add to Dashboard"))
          ),
          column(4, img(src = report$getImage() , width = "100%"))
        ),
        width = 12
      )
    )
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
 
