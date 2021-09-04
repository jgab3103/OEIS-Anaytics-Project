UI_fCreateOverviewTab <- function() {
   return(
      
     fluidPage(
       
       
       fluidRow(
         hr(),
         HTML("<b>Daily count of shifts for contact tracing and case management</b>"),
         column(width = 12,

                plotlyOutput("graph"),
                checkboxInput("viewMultiple", label = "Show line for aggregate of selected COVID response deployment", value = FALSE),   
         ),

       ),
       fluidRow(
         hr(),
         HTML("<b>Shift allocation (top 20 highest allocations)</b>"),
         column(width = 12,
                plotlyOutput("graph2")
         )
         
       )
       
     )
      
   )
  
}