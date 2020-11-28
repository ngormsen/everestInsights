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
    uiOutput(ns("reportElements")),
    valueBoxOutput(ns("myvaluebox")),
    valueBoxOutput(ns("numberOfCustomers")),
    box(shinycssloaders::withSpinner(plotOutput(ns("customerPerMonth")))),
    box(shinycssloaders::withSpinner(plotOutput(ns("revenuePerMonth")))),
    box(shinycssloaders::withSpinner(plotOutput(ns("revenuePerCustomerCohortDevelopment")))),
    box(shinycssloaders::withSpinner(plotOutput(ns("plotC3")))),
    box(shinycssloaders::withSpinner(plotlyOutput(ns("uniqueCustomerPerMonth")))),
    DTOutput(ns("plotTranslogRaw")),
    box(verbatimTextOutput(ns("mydata")))
  )
}
    
#' tabDashboardMain Server Function
#' 
#' @noRd 
mod_tabDashboardMain_server <- function(input, output, session, translog, translogClean, reports){
  ns <- session$ns
  
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
  
  
  output$reportElements <- renderUI({
    elements <- list()
    for(i in seq_along(reports)){
      if(reports[[i]]$getObject()$getDashboard() == TRUE){ # Reactive
        reports[[i]]$getInsightServer()
        elements[[i]] <- reports[[i]]$getInsightUi() # Link to submodule
        
      }
    }
    return(elements)
  })
  
}
    
## To be copied in the UI
# mod_tabDashboardMain_ui("tabDashboardMain_ui_1")
    
## To be copied in the server
# callModule(mod_tabDashboardMain_server, "tabDashboardMain_ui_1")
 
