#' @import R6 
CohortData <- R6Class("CohortData",
                     private = list(
                       rxTrigger = NULL,
                       randomText = NULL,
                       translog = NULL
                     ),
                     public = list(
                       initialize = function( translog ) {
                         private$rxTrigger = reactiveTrigger()
                         private$translog = translog
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
                       },
                       getTranslog = function(){
                         private$translog
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
