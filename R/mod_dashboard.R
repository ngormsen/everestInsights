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
      menuItem("Dashboard", tabName = "tabDashboard", icon = icon("dashboard")),
      menuItem("Cohort Analysis", tabName = "tabCohortAnalysis", icon = icon("th"))
    )
  )

  tabDashboard <- mod_tabDashboardMain_ui(ns("tabDashboardMain"))

  tabCohortAnalysis <- mod_tabDashboardCohortAnalysis_ui(ns("tabDashboardCohortAnalysis"))

  tagList(
    shinydashboard::dashboardPage(
      header = dashboardHeader( ),
      sidebar = sidebar,
      body = dashboardBody(
        tabItems(
          tabDashboard,
          tabCohortAnalysis
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

  # Call to submodules
  callModule(mod_tabDashboardMain_server, "tabDashboardMain", translog=translog, translogClean=translogClean)
  callModule(mod_tabDashboardCohortAnalysis_server, "tabDashboardCohortAnalysis", translog=translog, translogClean=translogClean)
}
    
## To be copied in the UI
# mod_dashboard_ui("dashboard_ui_1")
    
## To be copied in the server
# callModule(mod_dashboard_server, "dashboard_ui_1")
 