#' analysisChurn UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @import shinyjs stargazer survival survminer ggfortify
#' @importFrom shiny NS tagList 
mod_analysisChurnView_ui <- function(id){
  ns <- NS(id)
  
  tabItem(
    tabName = "analysisChurn",
    fluidRow(
      box(width = 12, htmlOutput(ns("researchQuestions"))),
      box(width = 12,
          img(src = "www/report_churn_2.png", width = "100%"),
          img(src = "www/report_churn_3.png", width = "100%"),
          img(src = "www/report_churn_4.png", width = "100%"),
          
          ChurnDefinition(inputId = ns("churnDefinition")),
          LookAtData(pltOutputId = ns("survTimeDist"))
      ),
      box(width = 12,
          tags$h3("Survival Model"),
          selectInput(ns("regressors"), label = "Select Regressors", choices = c("age", "salary", "acqChannel"), selected = "acqChannel", multiple = TRUE),
          verbatimTextOutput(ns("regressionTable")),
          selectInput(ns("survivalCurvesVariable"), label = "Filter", choices = c("age", "salary", "acqChannel"), selected = "acqChannel"),
          plotOutput(ns("survivalCurves"))
      )
  ))
}
    
#' analysisChurn Server Function
#'
#' @noRd 
mod_analysisChurnView_server <- function(input, output, session, report){
  ns <- session$ns
  reportData <- report$getReportData()
  translog <- reportData$getTranslog()
  
  survivalData <- reactive({
    translog <- AddSurvivalColumns(translog, churnDef = input$churnDefinition)
    translog <- AddFirstPurchaseProductCategoryColumn(translog)
    translog
  })
  
  observe({
    updateSelectInput(session = session,
                      inputId = "regressors",
                      choices = names(survivalData()),
                      selected = "acqChannel")
    updateSelectInput(session = session,
                      inputId = "survivalCurvesVariable",
                      choices = names(survivalData()),
                      selected = "acqChannel")
  })
  
  survivalModel <- reactive({
    regressors <- paste(input$regressors, collapse = "+")
    model <- coxph(formula = as.formula(paste0("Surv(survTime, isChurn) ~", regressors)),
                   data = survivalData())
  })
  
  output$regressionTable <- renderPrint({
    summary(survivalModel())
  })
  
  output$survivalCurves <- renderPlot({
    fit <- survfit(formula = as.formula(paste0("Surv(survTime, isChurn) ~", input$survivalCurvesVariable)),
                  data = survivalData())
    autoplot(fit)
  })
  
  output$researchQuestions <- renderUI(ResearchQuestionText())
}



    
## To be copied in the UI
# mod_analysisChurnView_ui("analysisChurn_ui")
    
## To be copied in the server
# callModule(mod_analysisChurnView_server, "analysisChurn")
 
