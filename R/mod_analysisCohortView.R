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
  
  tabItem(
    tabName = "analysisCohort",
    fluidRow(
      box(
        selectInput(ns("cohortType"), "Cohort Type", choices = list(Monthly="m", Quarterly="q", Yearly="y")),
        selectInput(ns("metric"), "Metric", choices = c("nCustomers", "revenue", "LTV30", "LTV60", "LTV30/CAC", "LTV60/CAC")),
        selectInput(ns("plotType"), "Chart", choices = c("table", "linechart")),
        selectInput(ns("view"), "View", choices = c(`Cohort Age`="CA", `Cohort Period`="CP")),
        plotOutput(ns("cohortTable")),
        title = "Visual Inspection",
        solidHeader = T,
        width = 12
      ),
      box(width = 12, title = "Regression",
          selectInput(ns("regressors"), label = "Regressors", choices = c("age", "period", "cohort"), selected = c("age", "cohort"), multiple = TRUE),
          fluidRow(
            column(6, uiOutput(ns("regressionTable")), uiOutput(ns("coefficientInterpretations"))),
            column(6, plotOutput(ns("regressionFit"))),
          )
      )
    )
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
  
  cohortData <- reactive({
    translog %>% 
      CohortType(cohortType = input$cohortType) %>% 
      CohortMetric(metric = input$metric) 
  })
  
  output$cohortTable <- renderPlot({
    cohortData() %>% 
      PlotCohort(plotType = input$plotType, view = input$view)
  })
  
  regressionModel <- reactive({
    regressors <- paste(input$regressors, collapse = "+")
    model <- lm(formula = as.formula(paste0("y ~ ", regressors)),
                data = cohortData())
  })
  
  output$regressionTable <- renderUI({
    tbl <- CreateRegressionHTMLTable(model = regressionModel(), 
                                     depVarLabel = input$metric)
    HTML(MarginTopBottom(tbl))
  })
  
  output$regressionFit <- renderPlot({
    cohortData() %>% 
      setDT() %>% 
      CreateDataForLinearModelFit(model = regressionModel()) %>% 
      PlotLinearModelFit()
  })
  
  output$coefficientInterpretations <- renderUI({
    HTML(InterpretCoefficients(model = regressionModel(), depVar = input$metric))
  })
  
}
    
## To be copied in the UI
# mod_analysisCohortView_ui("analysisCohortView_ui_1")
    
## To be copied in the server
# callModule(mod_analysisCohortView_server, "analysisCohortView_ui_1")
 

