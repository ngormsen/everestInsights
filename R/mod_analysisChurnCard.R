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
  tagList(
 
  )
}
    
#' analysisChurnCard Server Function
#'
#' @noRd 
mod_analysisChurnCard_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_analysisChurnCard_ui("analysisChurnCard_ui_1")
    
## To be copied in the server
# callModule(mod_analysisChurnCard_server, "analysisChurnCard_ui_1")
 
