#' analysisChurn UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @import shinyjs stargazer
#' @importFrom shiny NS tagList 
mod_analysisChurnView_ui <- function(id){
  ns <- NS(id)
  

  tabItem(
    tabName = "analysisChurn",
    fluidRow(
      actionButton(ns("randomTextButton"), "Activate Random Text"),
      box(title = "Actionable Insights", width = 12),
      box(width = 12, htmlOutput(ns("researchQuestions"))),
      box(width = 12,
          ChurnDefinition(inputId = ns("churnDefinition")),
          LookAtData(pltOutputId = ns("survTimeDist")),
          SelectPredictors(selectPredictorsInputId = ns("predictors")),
          tags$h4("Model Results"),
          fluidRow(
            column(6, htmlOutput(ns("regTable"))),
            column(6, plotOutput(ns("pltFit"))),
            column(6, plotOutput(ns("newPlot")))
            
          )
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
  
  output$researchQuestions <- renderUI({ResearchQuestionText()})

  output$survTimeDist <- renderPlot({
    hist(rnorm(20))
  })

  output$newPlot <- renderPlot({
    hist(translog()$amountSpent)
  })
  
  output$regTable <- renderUI({
    HTML(stargazer(lm(Sepal.Length ~ Species, data = iris), type = "html"))
  })

  output$pltFit <- renderPlot({
    ggfake_fit()
  })

  observeEvent(input$randomTextButton, {
    reportData$setRandomText("Roadrunner") 
  })
  
  updateSelectizeInput(session = session, inputId = "predictors", choices = c("hello", "world", "of", "science"))
  
}



    
## To be copied in the UI
# mod_analysisChurnView_ui("analysisChurn_ui")
    
## To be copied in the server
# callModule(mod_analysisChurnView_server, "analysisChurn")
 
