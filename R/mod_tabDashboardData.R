#' tabDashboardData UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tabDashboardData_ui <- function(id){
  ns <- NS(id)
  
  tabItem(
    tabName = "tabData",
    fluidRow(
      box(
        fileInput(
          ns("csvFile"),
          label = "Import Transaction Log",
          multiple = F,
          width = "250px",
          accept = ".csv"
        ),
        width = 12,
        title = "Hello"
      ),
      box(
        fluidRow(
          column(4, htmlOutput(ns("colnames"))),
          column(4,
                 h2("Required"),
                 selectizeInput(ns("customerId"), label = "Customer ID", choices = NULL, selected = NULL, multiple = FALSE),
                 selectizeInput(ns("amountSpent"), label = "Order Value ($)", choices = NULL, selected = NULL, multiple = FALSE),
                 selectizeInput(ns("orderTimestamp"), label = "Order Timestamp", choices = NULL, selected = NULL, multiple = FALSE),
                 actionButton(ns("setRequiredVariables"), label = "Set Columns")
          ),
          column(4,
                 h2("Optional"),
                 selectizeInput(ns("adSpent"), label = "Customer Acquisition Cost", choices = NULL, selected = NULL, multiple = FALSE),
                 selectizeInput(ns("productName"), label = "Product Name", choices = NULL, selected = NULL, multiple = FALSE),
                 actionButton(ns("setOptionalVariables"), label = "Set Columns")
          )
        ),
        width = 12
      ),
      box(
        DTOutput(ns('rawData')),
        width = 12,
        title = "World"
      )
    )
  )
}
    
#' tabDashboardData Server Function
#'
#' @noRd 
#' @import shinipsum
mod_tabDashboardData_server <- function(input, output, session){
  ns <- session$ns

  data <- reactive({
    csvFile <- input$csvFile
    if (is.null(csvFile)) {
      return(NULL)
    } else {
      return(as.data.table(read.csv(csvFile$datapath, header = TRUE)))
    }
  })
  
  observe({
    newChoices <- c("---", colnames(data()))
    updateSelectizeInput(session, "customerId",     choices = newChoices, selected = newChoices[1])
    updateSelectizeInput(session, "amountSpent",    choices = newChoices, selected = newChoices[1])
    updateSelectizeInput(session, "orderTimestamp", choices = newChoices, selected = newChoices[1])
    updateSelectizeInput(session, "adSpent",        choices = newChoices, selected = newChoices[1])
    updateSelectizeInput(session, "productName",    choices = newChoices, selected = newChoices[1])
  })
  
  output$colnames <- renderUI({
    tagList(
      tags$h2("Available Columns"),
      GenerateHtmlList(colnames(data()))
    )
  })
  
  output$rawData <- renderDT({
    DT::datatable(data()[1:20]) 
  })
  
  return(data)
}

GenerateHtmlList <- function(xs){
  out <- lapply(xs, function(x){
    tags$li(x)
  })
  return(tags$ul(out))
}
    
## To be copied in the UI
# mod_tabDashboardData_ui("tabDashboardData_ui_1")
    
## To be copied in the server
# callModule(mod_tabDashboardData_server, "tabDashboardData_ui_1")
 
