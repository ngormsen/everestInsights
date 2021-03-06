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

ChurnDefinition <- function(inputId){
  tagList(
    tags$h4("Defining Churn"),
    tags$p("Customers who have not made a purchase in the last ... days."),
    numericInput(inputId = inputId, label = "Number of Days", value = 60)
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

AddSurvivalColumns <- function(translog, churnDef){
  translog[, lastPurchaseDate := max(orderTimestamp), by = customerId]
  translog[, endOfStudyDate := max(orderTimestamp)]
  translog[, isChurn := ifelse(endOfStudyDate - lastPurchaseDate >= churnDef, 1, 0)]
  
  # Length of relationship
  translog[, survTime := endOfStudyDate - min(orderTimestamp), by = customerId]
  translog[isChurn == 1, 
           survTime := as.numeric(max(orderTimestamp) - min(orderTimestamp)),
           by = customerId]
  return(translog)
}

AddFirstPurchaseProductCategoryColumn <- function(translog){
  translog <- translog %>% 
    group_by(customerId) %>%
    summarise(
      orderTimestamp = min(orderTimestamp)
    ) %>% 
    left_join(translog, by = c("customerId", "orderTimestamp")) %>% 
    select(customerId, productCategory) %>% 
    rename(firstPurchaseProductCategory = productCategory) %>% 
    right_join(translog, by = "customerId") %>% 
    setDT()
  return(translog)
}