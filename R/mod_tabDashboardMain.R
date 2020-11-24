#' tabDashboardMain UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @import plotly lubridate
#' @importFrom DT DTOutput renderDT
#' @importFrom shiny NS tagList 
mod_tabDashboardMain_ui <- function(id){
  ns <- NS(id)
  
  tabItem(
    tabName = "tabDashboard",
    valueBoxOutput(ns("myvaluebox")),
    valueBoxOutput(ns("numberOfCustomers")),
    box(plotOutput(ns("customerPerMonth"))),
    box(plotOutput(ns("revenuePerMonth"))),
    box(plotOutput(ns("revenuePerCustomerCohortDevelopment"))),
    box(plotOutput(ns("plotC3"))),
    box(plotlyOutput(ns("uniqueCustomerPerMonth"))),
    DTOutput(ns("plotTranslogRaw")),
    box(verbatimTextOutput(ns("mydata")))
  )
}
    
#' tabDashboardMain Server Function
#' 
#' @noRd 
mod_tabDashboardMain_server <- function(input, output, session, translog, translogClean, mydata){
  ns <- session$ns
  
  output$plotTranslogRaw <- renderDT({
    PlotTranslog(translogClean())
  })
  
  output$mydata <- renderText({
    mydata()
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
  
  
}
    
## To be copied in the UI
# mod_tabDashboardMain_ui("tabDashboardMain_ui_1")
    
## To be copied in the server
# callModule(mod_tabDashboardMain_server, "tabDashboardMain_ui_1")
 
