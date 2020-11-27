#' @import R6 
#' @export
Report <- R6Class("Report",
   private = list(
      rxTrigger = NULL,
     .title = NULL,
     .text = NULL,
     .dashboard = NULL,
     .image = NULL,
     submoduleUi = NULL, 
     submoduleServer = NULL
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
   # callModule(mod_analysisChurnDashboard_server, "analysisChurnDashboard_ui"),
   # mod_analysisChurnDashboard_ui(ns("analysisChurnDashboard_ui")))

   public = list(
     initialize = function( title, submoduleServer, submoduleUi, nsId, id ) {
       private$rxTrigger = reactiveTrigger()
        
       private$.title <- title
       private$.text <- random_text(nwords=50)
       private$.dashboard <- FALSE
       private$.image <- random_ggplot(type = "col") + 
         labs(title = "Random plot") + 
         theme_bw()
       private$submoduleServer = callModule(submoduleServer, id, self)
       private$submoduleUi = submoduleUi(nsId)
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
     getServer = function(){
        private$submoduleServer
     },
     getUi = function(){
        private$submoduleUi
     },
     setServer = function(submoduleServer){
        private$submoduleServer = submoduleServer
     },
     setUi = function(submoduleUi){
        private$submoduleUi = submoduleUi
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
