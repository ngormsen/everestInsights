#' @import R6 
ChurnData <- R6Class("ChurnData",
                  private = list(
                    rxTrigger = NULL,
                    randomText = NULL
                    ),
                  public = list(
                    initialize = function( ) {
                      private$rxTrigger = reactiveTrigger()
                    },
                    getObject = function(){
                      private$rxTrigger$depend()
                      return(self) # TODO delete 'return' self
                    },
                    setRandomText = function(randomText){
                      private$rxTrigger$trigger()
                      private$randomText = randomText
                    },
                    getRandomText = function(){
                      private$rxTrigger$depend()
                      private$randomText
                    }
                  )
)

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
