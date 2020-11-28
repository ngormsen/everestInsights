#' @import R6 
#' @export
Report <- R6Class("Report",
   private = list(
      rxTrigger = NULL,
     .title = NULL,
     .text = NULL,
     .dashboard = NULL,
     .image = NULL,
     insightServer = NULL,
     insightUi = NULL,
     viewServer = NULL,
     viewUi = NULL
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
     initialize = function( title, ns, 
                            insightServer, insightUi, insightId, 
                            viewServer, viewUi, viewId ) {
       private$rxTrigger = reactiveTrigger()
       private$.title <- title
       private$.text <- random_text(nwords=50)
       private$.dashboard <- FALSE
       private$.image <- random_ggplot(type = "col") + 
         labs(title = "Random plot") + 
         theme_bw()
       private$insightServer = callModule(insightServer, insightId, self)
       private$insightUi = insightUi(ns(insightId))
       private$viewServer = callModule(viewServer, viewId, self)
       private$viewUi = viewUi(ns(viewId))
       
     },
     getObject = function(){
        private$rxTrigger$depend()
        return(self)
     },
     activateDashboard = function(){
       private$rxTrigger$trigger()
       private$.dashboard <- TRUE
     }, 
     deactivateDashboard = function(){
        private$rxTrigger$trigger()
        private$.dashboard <- FALSE
     }, 
     getDashboard = function(){
        private$.dashboard
     },
     getTitle = function(){
        private$.title
     },
     getInsightServer = function(){
        private$insightServer
     },
     getInsightUi = function(){
        private$insightUi
     },
     setInsightServer = function(insightServer){
        private$insightServer = insightServer
     },
     setInsightUi = function(insightUi){
        private$insightUi = insightUi
     }, 
     getViewServer = function(){
        private$viewServer
     },
     getViewUi = function(){
        private$viewUi
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
