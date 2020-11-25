#' @import R6 
#' @export
Report <- R6Class("Report",
   private = list(
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
       private$.title <- title
       private$.text <- random_text(nwords=50)
       private$.dashboard <- FALSE
       private$.image <- random_ggplot(type = "col") + 
         labs(title = "Random plot") + 
         theme_bw()
     },
     activateDashboard = function(){
       print("dashboard ist now true")
       private$.dashboard <- TRUE
       print(private$.dashboard)
     }
   )
)