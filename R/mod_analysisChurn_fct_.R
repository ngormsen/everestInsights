ResearchQuestionText <- function(){
  shiny::tagList(
    tags$h4("Research Questions"),
    GenerateHtmlList(c(
      "Which customers are churning?",
      "What are common characteristics among customers who churn?"
    )),
    tags$h4("Implications"),
    GenerateHtmlList(c(
      "Building a profile of long-term customers. Should be used for better targeting ads",
      "Identifying the most important predictor of churners and non-churners",
      "Deciding which acquisition channels are the best to acquire long-term customers"
    ))
  )
}

DefiningChurnText <- function(){
  shiny::tagList(
    tags$h4("Defining Customer Churn"),
    tags$p("First, we have to specify which customers are considered churners.")
  )
}


numericInput2 <- function(id, value, label){
  glue("
      <div class='myclass'>
        <label class='control-label' for='{label}'></label>
        <input id='{id}' type='number' class='form-control' value='{value}'/>
        </div>
      ")
}

ChurnDefinitionLabel <- function(nDays){
  glue("Number of Customers who have not made a purchase in the last {nDays} days")
}

ChurnDefinition <- function(inputId){
  tagList(
    tags$h4("Defining Churn"),
    tags$table(
      tags$tr(width = "100%",
              tags$td(tags$p("Customers who have not made a purchase in the last")),
              tags$td(tags$p(style = "padding: 0 4px", " ")),
              tags$td(numericInput(inputId = inputId, label = NULL, value = 60)),
              tags$td(tags$p(style = "padding: 0 4px", " ")),
              tags$td(tags$p("days.")))
    )
  )
}

LookAtData <- function(pltOutputId){
  tagList(
    tags$h4("Your Data"),
    tags$p("Based on your above definition of customer churn, we have built the 'Survival Time' for each customer."),
    plotOutput(outputId = pltOutputId)
  )
}

SelectPredictors <- function(selectPredictorsInputId){
  tagList(
    tags$h4("Set Up The Model"),
    tags$p("Your dataset consists of X variables. Please select the regressors that should be considered in the churn model."),
    selectizeInput(inputId = selectPredictorsInputId, label = NULL, multiple = T, choices = NULL, selected = NULL)
  )
}

SurvivalModel <- function(pltRegTableInputId, pltFitInputId){
  tagList(
    tags$h4("Model Result"),
    HTML(stargazer(lm(Sepal.Length ~ Species, data = iris), type = "html")),
    
  )
}