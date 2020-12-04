

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
    uiOutput(ns("header")),
    uiOutput(ns("insight")),
    uiOutput(ns("view"))
    
  )
}
    
#' reportView Server Function
#'
#' @noRd 
mod_reportViewHolder_server <- function(input, output, session, report){
  ns <- session$ns
  
  output$header <- renderUI(
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
                   )
            ),
            column(4, img(src = report$getImage() , width = "100%"))
          ),
          width = 12
      )
    )
  )
  
  output$insight <- renderUI({
    report$getViewInsightServer()
    report$getViewInsightUi()
  })
  output$view <- renderUI({
    report$getViewServer()
    report$getViewUi()
  })
  
  
  output$title <- renderUI({
    tags$h2(report$title)
  })
  
  output$text <- renderUI({
    paste0(report$text, sep = "\n")
  })
  
  output$image <- renderUI({
    img(src = "www/report_churn_2.png", width = "100%")
  })
  
}
    
## To be copied in the UI
# mod_reportView_ui("reportView_ui_1")
    
## To be copied in the server
# callModule(mod_reportView_server, "reportView_ui_1")
 
