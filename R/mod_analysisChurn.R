#' analysisChurn UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @import shinyjs stargazer
#' @importFrom shiny NS tagList 
mod_analysisChurn_ui <- function(id){
  ns <- NS(id)
  

  tabItem(
    tabName = "analysisChurn",
    fluidRow(
      box(title = "Actionable Insights", width = 12),
      box(width = 12, htmlOutput(ns("researchQuestions"))),
      box(width = 12,
          ChurnDefinition(inputId = ns("churnDefinition")),
          LookAtData(pltOutputId = ns("survTimeDist")),
          SelectPredictors(selectPredictorsInputId = ns("predictors")),
          tags$h4("Model Results"),
          fluidRow(
            column(6, htmlOutput(ns("regTable"))),
            column(6, plotOutput(ns("pltFit")))
          )
      )
  ))
}
    
#' analysisChurn Server Function
#'
#' @noRd 
mod_analysisChurn_server <- function(input, output, session, report){
  ns <- session$ns
  
  output$researchQuestions <- renderUI({ResearchQuestionText()})

  output$survTimeDist <- renderPlot({
    hist(rnorm(20))
  })

  output$regTable <- renderUI({
    HTML(stargazer(lm(Sepal.Length ~ Species, data = iris), type = "html"))
  })

  output$pltFit <- renderPlot({
    ggfake_fit()
  })

  updateSelectizeInput(session = session, inputId = "predictors", choices = c("hello", "world", "of", "science"))
  
}



    
## To be copied in the UI
# mod_analysisChurn_ui("analysisChurn_ui")
    
## To be copied in the server
# callModule(mod_analysisChurn_server, "analysisChurn")
 
