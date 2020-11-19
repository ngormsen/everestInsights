library(shinydashboard)
library(DT)
library(plotly)
library(data.table)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(shinipsum)

# For displaying Regression Output
library(sjPlot)
library(sjmisc)
library(sjlabelled)


#' dashboard UI Function
#' 
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import data.table
#' @importFrom shiny NS tagList 
mod_dashboard_ui <- function(id){
  ns <- NS(id)
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "tabDashboard", icon = icon("dashboard"))
      # menuItem("Cohort Analysis", tabName = "tabCohortAnalysis", icon = icon("th"))
    )
  )

  tabDashboard <- tabItem(
    tabName = "tabDashboard",
    valueBoxOutput(ns("myvaluebox")),
    valueBoxOutput(ns("numberOfCustomers")),
    box(plotOutput(ns("customerPerMonth"))),
    box(plotOutput(ns("revenuePerMonth"))),
    box(plotOutput(ns("revenuePerCustomerCohortDevelopment"))),
    box(plotOutput(ns("plotC3"))),
    box(plotlyOutput(ns("uniqueCustomerPerMonth"))),
    DTOutput(outputId=ns("plotTranslogRaw")),
  )

  # tabCohortAnalysis <- tabItem(
  #   tabName = "tabCohortAnalysis",
  #   fluidRow(
  #     box(
  #       fluidRow(
  #         column(4, selectInput("selectSummariseVar", "Variable", choices = c("amountSpent"))),
  #         column(4, selectInput("selectSummariseFunc", "Function", choices = c("mean", "median", "max", "min", "sum", "n_distinct"))),
  #         column(4, selectInput("selectRelativeTo", "Relative To", choices = c("none", "acq", "prev")))
  #       ),
  #       plotOutput("cohortTableCustom"),
  #       title = "Cohort Analysis",
  #       solidHeader = T,
  #       width = 12,
  #       height = "850px"
  #     ),
  #     box(
  #       fluidRow(
  #         column(2,
  #                selectInput("lmPredictors",
  #                            label = "Predictors",
  #                            choices = c("age", "cohort", "period", "as.factor(age)", "as.factor(cohort)", "as.factor(period)"),
  #                            multiple = T),
  #                actionButton("lmRun", label = "Run")
  #         ),
  #         column(5,
  #                htmlOutput("lm"),
  #                htmlOutput("lmInsights"),
  #         ),
  #         column(5,
  #                plotOutput("lmFit")
  #         )
  #       ),
  #       title = "Simple Linear Regression",
  #       width = 12
  #     )
  #   )
  # )

  tagList(
    shinydashboard::dashboardPage(
      header = dashboardHeader(

      ),
      sidebar = sidebar,
      body = dashboardBody(
        tabItems(
          tabDashboard
          # tabCohortAnalysis
        )
        
      ),
      title = "Hello Dashboard"
    ),
  )
  
}
    
#' dashboard Server Function
#'
#' @noRd 
mod_dashboard_server <- function(input, output, session){
  ns <- session$ns
  assignInNamespace("cedta.pkgEvalsUserCode", c(data.table:::cedta.pkgEvalsUserCode, "rtvs"), "data.table")
  
  # Data Import + Preprocessing --------------------------------------------------------------------
     translogRaw <- reactive({
    csvTable <- as.data.table(read.csv("data/retail_relay2.csv"))
  })

  # for dev: translogClean is already loaded into environment
  translogClean <- reactive({
    PreprocessRawTransactionLog(
      data = translogRaw(),
      columns = list("custId" = "customerId",
                     "amountSpent" = "amountSpent",
                     "orderTimestamp" = "orderTimestamp")
    )
  })

  translog <- reactive({
    CreateCohortCols(data = translogClean(), cohortType = "Monthly Cohorts")
  })

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

  # Plots DashBoard----------------------------------------------------------
  output$plotTranslogRaw <- renderDT({
    PlotTranslog(translogClean())
  })

  output$myvaluebox <- renderValueBox({
    valueBox(
      value = paste0(round(mean(translog()$amountSpent), 2), " $"),
      subtitle = "Avg. Order Size"
    )
  })

  numberOfCustomers <- reactive({
    length(unique(translogClean()$custId))
  })

  output$customerPerMonth <- renderPlot({
    ggplot(data = computeDataPerMonth(translog())[["customerPerMonth"]], aes(x = orderPeriod, y = numOrdersPerMonth)) +
      geom_bar(stat = "identity", fill = "steelblue", color = "white", width = 0.5) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 50, size = 8)) +
      ggtitle(paste("Number of Customers per Month")) +
      ylab("Number of Customers")
  })

    output$revenuePerMonth <- renderPlot({
      ggplot(data = computeDataPerMonth(translog())[["revenuePerMonth"]], aes(x = orderPeriod, y = amountSpentPerOrderPeriod)) +
        geom_bar(stat = "identity", fill = "steelblue", color = "white", width = 0.5) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 50, size = 8)) +
        ggtitle(paste("Revenue Per Month")) +
        ylab("Revenue Per Month")
    })

  dt <- reactive({
    CreateCohortCols(data = translogClean(), cohortType = "Monthly Cohorts")
  })

  output$revenuePerCustomerCohortDevelopment <- renderPlot({
    PlotCohortAgeLinechart(dt())
  })

  output$uniqueCustomerPerMonth <- renderPlotly({
    plotUniqueCustomerPerMonth(computeUniqueCustomerPerMonth(translog()))
  })

  output$numberOfCustomers <- renderValueBox({
    valueBox(
      value = numberOfCustomers(),
      subtitle = "numberOfCustomers",
      icon = icon("credit-card")
    )
  })


  output$plotC3 <- renderPlot({
    PlotC3(dt(), "Monthly Cohorts")
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
# mod_dashboard_ui("dashboard_ui_1")
    
## To be copied in the server
# callModule(mod_dashboard_server, "dashboard_ui_1")
 
