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
  box(
    fluidRow(
        selectInput(ns("cohortType"), "Cohort Type", choices = list(Monthly="m", Quarterly="q", Yearly="y")),
        selectInput(ns("metric"), "Metric", choices = c("nCustomers", "revenue", "LTV30", "LTV60", "LTV30/CAC", "LTV60/CAC")),
        selectInput(ns("plotType"), "Chart", choices = c("table", "linechart")),
        selectInput(ns("view"), "View", choices = c(`Cohort Age`="CA", `Cohort Period`="CP"))
        
    ),
    plotOutput(ns("cohortTable")),
    title = "Visual Inspection",
    solidHeader = T,
    width = 12,
    height = "850px"
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
  
  
  
  output$cohortTable <- renderPlot({
    translog %>% 
      CohortType(cohortType = input$cohortType) %>% 
      CohortMetric(metric = input$metric) %>% 
      PlotCohort(plotType = input$plotType, view = input$view)
  })
  
}
    
## To be copied in the UI
# mod_analysisCohortView_ui("analysisCohortView_ui_1")
    
## To be copied in the server
# callModule(mod_analysisCohortView_server, "analysisCohortView_ui_1")
 
