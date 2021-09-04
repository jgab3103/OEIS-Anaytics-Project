UIfCreateSideBar <- function(pstaffData, pdf) {
  

    return(

      
      sidebarPanel(
        
        selectInput("selectBusinessUnit", label ="Business Unit", multiple = TRUE, 
                    choices = pstaffData$Division,  selected = pstaffData$Division),
        selectInput("selectTeam", label ="Team", multiple = TRUE, 
                    choices = pstaffData$Team, 
                    selected = pstaffData$Team),
        dateRangeInput("dates", label = "Date range" ,
                       start = as.Date("2021-07-12"),
                       end = max(pdf$Date)),
        checkboxInput("contactTracing", label = "Contact Tracing", value = TRUE),
        checkboxInput("caseManagement", label = "Case Management", value = FALSE),   
        checkboxInput("otherRedeployment", label = "Other redeployment", value = FALSE),   

        
        htmlOutput('blurb'),
        br(),
        downloadButton("aggData", "Download data tables")
        
        )
      
    )
  
  
  
  
  
  
}