library(shiny)

reactiveTrigger <- function() {
  counter <- reactiveVal( 0)
  list(
    depend = function() {
      counter()
      invisible()
    },
    trigger = function() {
      counter( isolate(counter()) + 1 )
    }
  )
}

counter <- R6::R6Class(
  public = list(
    initialize = function(reactive = FALSE) {
      private$reactive = reactive
      private$value = 0
      private$trueValue = FALSE
      private$rxTrigger = reactiveTrigger()
    },
    setIncrement = function() {
      if (private$reactive) private$rxTrigger$trigger()
      private$value = private$value + 1
    },
    setDecrement = function() {
      if (private$reactive) private$rxTrigger$trigger()
      private$value = private$value -1
    },
    setTrue = function(){
      if (private$reactive) private$rxTrigger$trigger()
      private$trueValue = TRUE
    },
    getValue = function() {
      return(private$value)
    },
    getTrueValue = function(){
      return(private$trueValue)
    },
    getObject = function(){
      if (private$reactive) private$rxTrigger$depend()
      return(self)
    }
  
  ),
  private = list(
    reactive = NULL,
    value = NULL,
    rxTrigger = NULL,
    trueValue = NULL
  )
)

ui <- fluidPage(
  actionButton("minus", "-1"),
  actionButton("plus", "+1"),
  actionButton("trigger", "Trigger"),
  textOutput("value"),
  textOutput("triggered")
)

server <- function(input, output, session) {
  count <- counter$new(reactive = TRUE)
  
  # observeEvent(input$minus, { count$setDecrement() })
  # observeEvent(input$plus,  { count$setIncrement() })
  observeEvent(input$trigger, {count$setTrue()})
  
  output$value <- renderText({ count$getValue() })
  output$triggered <- renderText({
    if(count$getObject()$getTrueValue() == TRUE){
      browser()
      count$getObject()$getValue()
    }
    else{
      ""
    }
    })
  
}

# shinyApp(ui, server)