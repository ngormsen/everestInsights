#' analysisCohortView UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_analysisCohortView_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' analysisCohortView Server Function
#'
#' @noRd 
mod_analysisCohortView_server <- function(input, output, session, report){
  ns <- session$ns
  reportData <- report$getReportData()
  translog <- reportData$getTranslog()
  translog <- AddAcquisitionTimestamp(translog, "orderTimestamp", "customerId")
}
    
## To be copied in the UI
# mod_analysisCohortView_ui("analysisCohortView_ui_1")
    
## To be copied in the server
# callModule(mod_analysisCohortView_server, "analysisCohortView_ui_1")
 
