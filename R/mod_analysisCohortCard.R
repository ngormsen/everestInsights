#' analysisCohortCard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_analysisCohortCard_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns("somehing"))
  uiOutput(ns("htmlOutput"))
}
    
#' analysisCohortCard Server Function
#'
#' @noRd 
mod_analysisCohortCard_server <- function(input, output, session, report, dashboardSession){
  ns <- session$ns
  output$title <- renderUI({
    tags$h2(report$title)
  })
  
  output$text <- renderUI({
    report$text
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
          column(4,shinycssloaders::withSpinner(plotOutput(ns("image"), height = "200px"), type = 7))
        ),
        width = 12
      )
    )
    
    # tags$div(class="tile", 
    #          tags$img(src = 'https://images.unsplash.com/photo-1464054313797-e27fb58e90a9?dpr=1&auto=format&crop=entropy&fit=crop&w=1500&h=996&q=80'),
    #          tags$div(class = "text",
    #                   tags$h1("Lorem ipsum"),
    #                   tags$h2(class="animate-text", "More lorem ipsum bacon ipsum."),
    #                   tags$p(class = "animate-text", "Bacon ipsum dolor amet pork belly tri-tip turd"),
    #                   actionButton(ns("reportButton"), "Open Report", class="animate-text"),
    #                   actionButton(ns("dashboardButton"), "Add to Dashboard", class="animate-text")
    #          )
    # )
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
# mod_analysisCohortCard_ui("analysisCohortCard_ui_1")
    
## To be copied in the server
# callModule(mod_analysisCohortCard_server, "analysisCohortCard_ui_1")
 
