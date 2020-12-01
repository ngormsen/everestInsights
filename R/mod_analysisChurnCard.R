#' analysisChurnCard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_analysisChurnCard_ui <- function(id){
  ns <- NS(id)
  
  fluidRow(
    column(1),
    column(7,
           fluidRow(
             # tags$h1(
             #   "Work plz"
             # ),
             textOutput(ns("title"))
           ),
           fluidRow(
             # uiOutput(ns("html")),
             uiOutput(ns("htmlOutput"))
           )
    ),
    column(4,shinycssloaders::withSpinner(plotOutput(ns("image"), height = "200px"), type = 7))
  )}
    
#' analysisChurnCard Server Function
#'
#' @noRd 
mod_analysisChurnCard_server <- function(input, output, session, report){
  ns <- session$ns
  
  output$title <- renderText({
    report$title
  })
  
  output$text <- renderText({
    report$text
  })
  
  output$image <- renderPlot({
    report$image
  })
  
  output$htmlOutput <- renderUI({
    tags$div(class="wrap",
             tags$div(class="tile", 
                tags$img(src = 'https://images.unsplash.com/photo-1464054313797-e27fb58e90a9?dpr=1&auto=format&crop=entropy&fit=crop&w=1500&h=996&q=80'),
                tags$div(class = "text",
                  tags$h1("Lorem ipsum"),
                  tags$h2(class="animate-text", "More lorem ipsum bacon ipsum."),
                  tags$p(class = "animate-text", "Bacon ipsum dolor amet pork belly tri-tip turd"),
                  tags$div(class = "dots",
                           tags$span(),
                           tags$span(),
                           tags$span(),                  
                           actionButton("Action", "action", class = "animate-text"),
                           actionButton("Action2", "action", class = "animate-text")
                           
)
             ))
             )
  })
  
  output$html <- renderUI({
    # tags$div(class = "header", checked = NA,
    #          tags$p("Ready to take the Shiny tutorial? If so"),
    #          tags$a(href = "shiny.rstudio.com/tutorial", "Click Here!")
    # )
    HTML(
      "
<div class='wrap'>
<div class='tile'> 
  <img src='https://images.unsplash.com/photo-1464054313797-e27fb58e90a9?dpr=1&auto=format&crop=entropy&fit=crop&w=1500&h=996&q=80'/>
  <div class='text'>
  <h1>Lorem ipsum.</h1>
  <h2 class='animate-text'>More lorem ipsum bacon ipsum.</h2>
  <p class='animate-text'>Bacon ipsum dolor amet pork belly tri-tip turducken, pancetta bresaola pork chicken meatloaf. Flank sirloin strip steak prosciutto kevin turducken. </p>
<div class='dots'>
    <span></span>
    <span></span>
    <span></span>
  </div>
  </div>
 </div>


<div class='tile'> 
  <img src='https://images.unsplash.com/photo-1458668383970-8ddd3927deed?dpr=1&auto=format&crop=entropy&fit=crop&w=1500&h=1004&q=80'/>
  <div class='text'>
  <h1>Lorem ipsum.</h1>
  <h2 class='animate-text'>Cohort Analysis</h2>
  <p class='animate-text'>Bacon ipsum dolor amet pork belly tri-tip turducken, pancetta bresaola pork chicken meatloaf. Flank sirloin strip steak prosciutto kevin turducken. </p>
<div class='dots'>
    <span></span>
    <span></span>
    <span></span>
  </div>
  </div>
 </div>
  
  <div class='tile'> 
  <img src='https://images.unsplash.com/photo-1422393462206-207b0fbd8d6b?dpr=1&auto=format&crop=entropy&fit=crop&w=1500&h=1000&q=80'/>
  <div class='text'>
  <h1>Lorem ipsum.</h1>
  <h2 class='animate-text'>More lorem ipsum bacon ipsum.</h2>
  <p class='animate-text'>Bacon ipsum dolor amet pork belly tri-tip turducken, pancetta bresaola pork chicken meatloaf. Flank sirloin strip steak prosciutto kevin turducken. </p>
<div class='dots'>
    <span></span>
    <span></span>
    <span></span>
  </div>
  </div>
 </div>
</div>
  
  
</div>      "
    )
  })
  
  observeEvent(input$dashboardButton, {
    if(report$getDashboard() == FALSE){
      report$activateDashboard()
      updateActionButton(session, "dashboardButton", label = "Remove from Dashboard")
    }
    else{
      report$deactivateDashboard()
      updateActionButton(session, "dashboardButton", label = "Add to Dashboard")
    }
  })
  
  observeEvent(input$reportButton, {
    updateTabItems(session = dashboardSession, "tabsMenu", report$getId())
  })
  
}
    
## To be copied in the UI
# mod_analysisChurnCard_ui("analysisChurnCard_ui_1")
    
## To be copied in the server
# callModule(mod_analysisChurnCard_server, "analysisChurnCard_ui_1")
 
