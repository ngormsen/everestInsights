

# 
#' dashboard UI Function
#' 
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import data.table
#' @import utils RColorBrewer dplyr 
#' @import dashboardthemes
#' @import shinydashboard
#' @importFrom shiny NS tagList 
mod_dashboard_ui <- function(id){
  ns <- NS(id)
  
  sidebar = dashboardSidebar(
    sidebarMenuOutput(ns("menu")),
    textOutput(ns("text") )
  )

  tagList(
    shinydashboard::dashboardPage(
      header = dashboardHeader( title = "EVEREST INSIGHT" ),
      sidebar = sidebar,
      body = dashboardBody(
        shinyDashboardThemes(
          theme = "poor_mans_flatly"
        ),
        
        uiOutput(ns("tabItems"))
      ),
      title = "Hello Dashboard"
    )
  )
}
    
#' dashboard Server Function
#' 
#' @noRd 
mod_dashboard_server <- function(input, output, session){
  ns <- session$ns
  
  # Generate Menu entries -----------------------------------------------------
  
  # Creating a new report:
  #   1. Create a new object report with
  #     a. Title
  #     b. ns
  #     c. card submodule (server, ui, id)
  #     d. insight element submodule (server, ui, id)
  #     e. report view submodule (server, ui, id)
  #     f. data object which holds the shared data between insight and view submodules (for each report different)
  # Implement the logic in the respective submodules

  # Call to static submodulest
  translogChurn <- callModule(mod_tabDashboardData_server, "tabData")
  translogCohort <- read_csv("data/fake_data.csv") %>%
    setDT()
  
  reports <- list(
    Report$new("Churn Analysis", "churnAnalysis", ns, ChurnData$new(translogChurn), session,
               mod_analysisChurnCard_server,
               mod_analysisChurnCard_ui,
               "analysisChurnCard",
               mod_analysisChurnInsight_server,
               mod_analysisChurnInsight_ui,   
               "analysisChurnInsight",
               mod_analysisChurnView_server,
               mod_analysisChurnView_ui,
               "analysisChurnView"
               ),
    Report$new("Cohort Analysis", "cohortAnaysis", ns, CohortData$new(translogCohort), session,
               mod_analysisCohortCard_server,
               mod_analysisCohortCard_ui,
               "analysisCohortCard",
               mod_analysisCohortInsight_server,
               mod_analysisCohortInsight_ui,
               "analysisCohortInsight",
               mod_analysisCohortView_server,
               mod_analysisCohortView_ui,
               "analysisCohortView"
               )
  )
  
  callModule(mod_tabDashboardMain_server, "tabDashboardMain", translog=translogChurn, translogClean=translogClean, reports=reports)
  callModule(mod_tabDashboardCohortAnalysis_server, "tabDashboardCohortAnalysis", translog=translogChurn)
  callModule(mod_tabDashboardReport_server, "tabDashboardReport", reports=reports, dashboardSession=session)
  
  
  
  reportsSubMenuEntries <- list()
  for(i in 1:length(reports)){
    reportsSubMenuEntries[[i]] <- menuSubItem(reports[[i]]$title,
                                              tabName = reports[[i]]$getId(),
                                              icon = icon('line-chart'))
  }
    

  # Adding the menu items of the sidebar
  output$menu <- renderMenu({
    sidebarMenu(
      id = ns("tabsMenu"),
      # Static menu entries
      menuItem("Data", tabName = "tabData"),
      menuItem("Dashboard", tabName = "tabDashboard", icon = icon("dashboard")),
      menuItem("Cohort Analysis", tabName = "tabCohortAnalysis", icon = icon("th")),
      # menuItem("Churn Analysis", tabName = "analysisChurn"),
      
      # The Reports menu item has several dynamically generated sub menu entries
      menuItem("Reports", tabName = "tabReports", icon = icon("file-alt"),
               menuSubItem("Cards", tabName = "tabReports", icon = icon("file-alt")),
               reportsSubMenuEntries # list with dynamically generated sub menu entries
         )
      )
  })
  
  reportsTabs <- list(
    mod_tabDashboardData_ui(ns("tabData")),
    mod_tabDashboardMain_ui(ns("tabDashboardMain")),
    mod_tabDashboardCohortAnalysis_ui(ns("tabDashboardCohortAnalysis")),
    mod_tabDashboardReport_ui(ns("tabDashboardReport"))
  )
  
  reportIdx <- 1
  for(i in (length(reportsTabs) + 1):(length(reportsTabs) + length(reports))){
    report <- reports[[reportIdx]]
    reportsTabs[[i]] <- mod_reportViewHolder_ui(ns(report$getId()), report$getId())
    reportIdx <- reportIdx + 1
  }
  
  output$tabItems <- renderUI({
    do.call(tabItems,
            reportsTabs
  )})
  
  

  # Call all dynamically generated report submodules
  lapply(seq_along(reports),
         function(i){
           callModule(
             mod_reportViewHolder_server,
             reports[[i]]$getId(),
             reports[[i]]
           )
       })
  
}
    
## To be copied in the UI
# mod_dashboard_ui("dashboard_ui_1")
    
## To be copied in the server
# callModule(mod_dashboard_server, "dashboard_ui_1")
 
