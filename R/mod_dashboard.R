

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
      header = dashboardHeader( ),
      sidebar = sidebar,
      body = dashboardBody(
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
  
  # Data Import + Preprocessing --------------------------------------------------------------------

  createDataObject <- reactive({
    dataHandler <- DataHandler$new()
    dataHandler$preprocessTransactionLog()
    dataHandler$createCohortStructure()
  })
  
  translogRaw <- reactive({
    dataObject <- createDataObject()
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

  # Generate Menu entries -----------------------------------------------------
  

  reports <- list(
    # Report$new("Cohort Analysis")
    Report$new("Churn Analysis", 
               mod_analysisChurnDashboard_server, 
               mod_analysisChurnDashboard_ui, 
               ns("analysisChurnDashboard_ui"), # Find another solution. This works but is formally incorrect.
               "analysisChurnDashboard_ui")
  )
  
  
  reportsSubMenuEntries <- list()

  for(i in 1:length(reports)){
    reportsSubMenuEntries[[i]] <- menuSubItem(reports[[i]]$title, 
                                              tabName = paste0("report_",i), 
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
      menuItem("Churn Analysis", tabName = "analysisChurn"),
      
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
    mod_tabDashboardReport_ui(ns("tabDashboardReport")),
    mod_analysisChurn_ui(ns("analysisChurn"))
  )
  
  reportIdx <- 1
  for(i in (length(reportsTabs) + 1):(length(reportsTabs) + length(reports))){
    reportsTabs[[i]] <- mod_reportView_ui(ns(paste0("reportView_ui_",reportIdx)), reportIdx)
    reportIdx <- reportIdx + 1
  }
  
  output$tabItems <- renderUI({
    do.call(tabItems,
            reportsTabs
  )})
  
  
  # Call to submodules
  data <- callModule(mod_tabDashboardData_server, "tabData")
  callModule(mod_tabDashboardMain_server, "tabDashboardMain", translog=translog, translogClean=translogClean, reports=reports)
  callModule(mod_tabDashboardCohortAnalysis_server, "tabDashboardCohortAnalysis", translog=translog, translogClean=translogClean)
  callModule(mod_tabDashboardReport_server, "tabDashboardReport", reports=reports, dashboardSession=session)
  callModule(mod_analysisChurn_server, "analysisChurn")
  
  # Call all dynamically generated report submodules
  lapply(seq_along(reports),
         function(i){
           callModule(
             mod_reportView_server,
             paste0("reportView_ui_", i),
             reports[[i]]
           )
       })
  

  
}
    
## To be copied in the UI
# mod_dashboard_ui("dashboard_ui_1")
    
## To be copied in the server
# callModule(mod_dashboard_server, "dashboard_ui_1")
 
