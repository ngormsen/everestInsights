#' analysisCohortInsight UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_analysisCohortInsight_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' analysisCohortInsight Server Function
#'
#' @noRd 
mod_analysisCohortInsight_server <- function(input, output, session, report){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_analysisCohortInsight_ui("analysisCohortInsight_ui_1")
    
## To be copied in the server
# callModule(mod_analysisCohortInsight_server, "analysisCohortInsight_ui_1")
 
