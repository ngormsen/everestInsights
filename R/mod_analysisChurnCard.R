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
  output$title <- renderText({
    report$title
  })
  
  output$text <- renderText({
    report$text
  })
  
  output$image <- renderPlot({
    report$image
  })
  
  
  output$htmlOutput <- renderUI({
       tags$div(class="tile", 
          tags$img(src = 'https://images.unsplash.com/photo-1464054313797-e27fb58e90a9?dpr=1&auto=format&crop=entropy&fit=crop&w=1500&h=996&q=80'),
          tags$div(class = "text",
            tags$h1("Lorem ipsum"),
            tags$h2(class="animate-text", "More lorem ipsum bacon ipsum."),
            tags$p(class = "animate-text", "Bacon ipsum dolor amet pork belly tri-tip turd"),
             actionButton(ns("reportButton"), "Open Report", class="animate-text"),
             actionButton(ns("dashboardButton"), "Add to Dashboard", class="animate-text")
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
 
