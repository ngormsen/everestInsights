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
        fluidRow(
          column(4, 
            fileInput(
              ns("translogCSV"),
              label = "Import Transaction Log",
              multiple = F,
              width = "250px",
              accept = ".csv"
            )
          ),
          column(4,
            fileInput(
              ns("otherCSV1"),
              label = "Other CSV 1",
              multiple = F,
              width = "250px",
              accept = ".csv"
            ),
            selectInput(ns("otherData1joinVars"), label = "Join on...", choices = NULL, multiple = T),
            actionButton(ns("joinWithTranslog1"), label = "Append to Translog")
          ),
          column(4,
            fileInput(
              ns("otherCSV2"),
              label = "Other CSV 2",
              multiple = F,
              width = "250px",
              accept = ".csv"
            ),
            selectInput(ns("otherData2joinVars"), label = "Join on...", choices = NULL, multiple = T),
            actionButton(ns("joinWithTranslog2"), label = "Append to Translog")
          )
        ),
        width = 12,
        title = "Import Datasets"
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
  
  data <- reactiveVal(value = NULL, label = "data")
  
  observe({
    translog <- GetCSVFromUserInput(input = input$translogCSV)
    data(translog)
  })
  
  otherData1 <- reactive({
    GetCSVFromUserInput(input = input$otherCSV1)
  })
  
  otherData2 <- reactive({
    GetCSVFromUserInput(input = input$otherCSV2)
  })
  
  output$colnames <- renderUI({
    tagList(
      tags$h2("Available Columns"),
      GenerateHtmlList(colnames(data()))
    )
  })
  
  observe({
    newChoices <- c("---", colnames(data()))
    updateSelectizeInput(session, "customerId",     choices = newChoices, selected = newChoices[1])
    updateSelectizeInput(session, "amountSpent",    choices = newChoices, selected = newChoices[1])
    updateSelectizeInput(session, "orderTimestamp", choices = newChoices, selected = newChoices[1])
    updateSelectizeInput(session, "adSpent",        choices = newChoices, selected = newChoices[1])
    updateSelectizeInput(session, "productName",    choices = newChoices, selected = newChoices[1])
  })
  
  observe({
    newChoices <- colnames(otherData1())
    commonCols <- FindCommonColumns(dt1 = data(), dt2 = otherData1())
    updateSelectInput(session, "otherData1joinVars", choices = newChoices, selected = commonCols)
  })
  
  observe({
    newChoices <- colnames(otherData2())
    commonCols <- FindCommonColumns(dt1 = data(), dt2 = otherData2())
    updateSelectInput(session, "otherData2joinVars", choices = newChoices, selected = commonCols)
  })
  
  observeEvent(input$joinWithTranslog1, {
    data(JoinTranslogWithData(translog = data(), data = otherData1(), by = input$otherData1joinVars))
  })
  
  observeEvent(input$joinWithTranslog2, {
    data(JoinTranslogWithData(translog = data(), data = otherData2(), by = input$otherData2joinVars))
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
 
