#' @import R6 
#' @export
Report <- R6Class("Report",
   private = list(
      rxTrigger = NULL,
     .title = NULL,
     .text = NULL,
     .dashboard = NULL,
     .image = NULL
   ),
   active = list(
     text = function() {
       private$.text
     },
     dasboard = function(){
       private$.dashboard
     },
     image = function(){
       private$.image
     },
     title = function(){
       private$.title
     }
   ),
   public = list(
     initialize = function( title ) {
       private$rxTrigger = reactiveTrigger()
        
       private$.title <- title
       private$.text <- random_text(nwords=50)
       private$.dashboard <- FALSE
       private$.image <- random_ggplot(type = "col") + 
         labs(title = "Random plot") + 
         theme_bw()
     },
     getObject = function(){
        private$rxTrigger$depend()
        return(self)
     },
     activateDashboard = function(){
       private$rxTrigger$trigger()
       print("dashboard ist now true")
       private$.dashboard <- TRUE
       print(private$.dashboard)
       
     }, 
     getDashboard = function(){
        private$.dashboard
     },
     getTitle = function(){
        private$.title
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
