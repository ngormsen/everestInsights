#' tabDashboardCohortAnalysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tabDashboardCohortAnalysis_ui <- function(id){
  ns <- NS(id)
  
  tabItem(
    tabName = "tabCohortAnalysis",
    fluidRow(
      box(
        fluidRow(
          column(4, selectInput(ns("selectSummariseVar"), "Variable", choices = c("amountSpent"))),
          column(4, selectInput(ns("selectSummariseFunc"), "Function", choices = c("mean", "median", "max", "min", "sum", "n_distinct"))),
          column(4, selectInput(ns("selectRelativeTo"), "Relative To", choices = c("none", "acq", "prev")))
        ),
        plotOutput(ns("cohortTableCustom")),
        title = "Cohort Analysis",
        solidHeader = T,
        width = 12,
        height = "850px"
      ),
      box(
        fluidRow(
          column(2,
                 selectInput(ns("lmPredictors"),
                             label = "Predictors",
                             choices = c("age", "cohort", "period", "as.factor(age)", "as.factor(cohort)", "as.factor(period)"),
                             multiple = T),
                 actionButton(ns("lmRun"), label = "Run")
          ),
          column(5,
                 htmlOutput(ns("lm")),
                 htmlOutput(ns("lmInsights")),
          ),
          column(5,
                 plotOutput(ns("lmFit"))
          )
        ),
        title = "Simple Linear Regression",
        width = 12
      )
    )
  )
}
    
#' tabDashboardCohortAnalysis Server Function
#'
#' @noRd 
mod_tabDashboardCohortAnalysis_server <- function(input, output, session, translog, translogClean){
  ns <- session$ns
  # Computations ------------------------------------------------------------
  
  cohortTable <- reactive({
    GetDataCohortTableCustom(
      translog = translog(),
      x = "orderPeriod",
      var = input$selectSummariseVar,
      fun = input$selectSummariseFunc,
      relativeTo = input$selectRelativeTo
    )
  })
  
  linearModel <- eventReactive(input$lmRun, {
    ComputeLinearModel(data = cohortTable(), predictors = input$lmPredictors)
  })
  
  linearModelFit <- reactive({
    dtPlt <- CreateDataForLinearModelFit(model = linearModel()[["model"]],
                                         data = linearModel()[["data"]])
  })
  
  
  # Plots Cohort----------------------------------------------------------
  
  output$cohortTableNPurchases <- renderPlot({
    data <- GetDataCohortTableOfNumPurchases(translog(), x = "orderPeriod")
    PlotCohortTableOfNumPurchases(data)
  })
  
  output$cohortTableCustom <- renderPlot(expr = {
    if (input$selectRelativeTo == "none"){
      return(PlotCohortTableCustom(cohortTable(), perc = F))
    } else {
      return(PlotCohortTableCustom(cohortTable(), perc = T))
    }
  }, height = 600)
  
  output$lm <- renderUI({
    depVarLabel <- SetLabelForDepVar(
      summariseVar = input$selectSummariseVar,
      summariseFunc = input$selectSummariseFunc,
      relativeTo = input$selectRelativeTo
    )
    HTML(
      MarginTopBottom(
        CreateRegressionHTMLTable(linearModel()[["model"]], depVarLabel)
      )
    )
  })
  
  output$lmInsights <- renderUI({
    interpretations <- InterpretCoefficients(model = linearModel()[["model"]])
    HTML(interpretations)
  })
  
  output$lmFit <- renderPlot({
    PlotLinearModelFit(linearModelFit())
  })
  
}
    
## To be copied in the UI
# mod_tabDashboardCohortAnalysis_ui("tabDashboardCohortAnalysis_ui")
    
## To be copied in the server
# callModule(mod_tabDashboardCohortAnalysis_server, "tabDashboardCohortAnalysis_ui")
 
