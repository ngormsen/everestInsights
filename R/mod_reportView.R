

#' reportView UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @import knitr
#' @importFrom shiny NS tagList 
mod_reportView_ui <- function(id, idx){
  ns <- NS(id)
  
  tabItem(
    tabName = paste0("report_",idx),
    uiOutput(ns("markdown"))
  )
}
    
#' reportView Server Function
#'
#' @noRd 
mod_reportView_server <- function(input, output, session, report){
  ns <- session$ns
  
  output$markdown <- renderUI({
    # HTML(markdown::markdownToHTML(knit('Untitled1.Rmd', quiet = TRUE)))
    # includeMarkdown(rmarkdown::render('Untitled1.Rmd'))
  })
  
}
    
## To be copied in the UI
# mod_reportView_ui("reportView_ui_1")
    
## To be copied in the server
# callModule(mod_reportView_server, "reportView_ui_1")
 
