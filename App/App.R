# Load all libraries
library(shiny)
library(tidyverse)
library(ggplot2)
library(DT)
library(plotly)
library(lubridate)
library(writexl)
library(tsibble)
library(shinyjs)

# Settings
source("./R/fGetSettings.R")

# Data Manager
source("./R/DataManager.R")

# Helper functions
source("./R/fBuildTypeVectorFilter.R")

# UI Files
source("./R/UI_fCreateSideBar.R")
source("./R/UI_fCreateOverviewTab.R")

# Server Files
source("./R/Server_fCreateAppAuthorAndVersionDetails.R")
source("./R/Server_fCreateEnhancedDataSet.R")


# TO DO - move this stuff out of global
df <- read_csv("./Data/cleanedContractTracingShiftData.csv")
staffData <- read_csv("./Data/cleanedContactTracingStaffData.csv")
enhancedPersonLevelData <- merge(df, staffData  , by = "Person",  all.x = TRUE)
enhancedData <- Server_fCreateEnhancedDataSet(df)



# UI ---------------------------------------------
ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Cancer Institute Covid Response"),
  br(),
  sidebarLayout(
    UIfCreateSideBar(pstaffData = staffData,
                     pdf = df),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Overview", 
                           
                           UI_fCreateOverviewTab()
                           
                           ),

                  tabPanel("Data", 
                           br(),
                           HTML("<b>Staff profile data</b><br/>"),
                           br(),
                           DT::dataTableOutput("enhancedDataTable"),
                           
                           br(),
                           HTML("<b>Single shift data data</b><br/>"),
                           br(),
                           
                           DT::dataTableOutput("shiftData")
                           
                           ),
                  
                  
                  tabPanel("About", 
                           br(),
                           htmlOutput("AboutText"))
      )
      
    )
  )
)

# Server -------------------------------------------
server <- function(input, output, session) {
  
  
  # TO do - clean output names so they are more desciptive 
  
  # Reactive components --------------------------------

  output$graph <- renderPlotly({

    dates <- input$dates
    businessUnitFilter = input$selectBusinessUnit
    teamFilter = input$selectTeam
    contacttracingFilter = input$contactTracing
    casemanagementFilter = input$caseManagement
    otherredploymentFilter = input$otherRedeployment
    viewMultiple <- input$viewMultiple
    

    
    combinedtypeVectorFiltered <- fBuildTypeVectorFilter(pcontacttracingFilter = contacttracingFilter,
                                                              pcasemanagementFilter = casemanagementFilter,
                                                              potherredploymentFilter = otherredploymentFilter)
    
    CombinedColourLabel <- paste0(combinedtypeVectorFiltered, collapse = " + ")
    
  
  # FILTER DATA -----------------------------------------    
    df2 <- enhancedPersonLevelData %>%
      filter(Type %in% combinedtypeVectorFiltered) %>%
      filter(Division %in% businessUnitFilter) %>%
      filter(Team %in% teamFilter) %>%
      filter(Date >= dates[1], Date <= dates[2]) %>%
      group_by(Date) %>%
      summarize(counts = n()) %>%
      tsibble(index = Date) %>%
      fill_gaps(., .full = TRUE) %>%
      tibble() %>%
      mutate(counts = replace_na(counts, 0))
    
    dfContactTracing <- enhancedPersonLevelData %>%
      filter(Type == "Contact tracing") %>%
      filter(Division %in% businessUnitFilter) %>%
      filter(Team %in% teamFilter) %>%
      filter(Date >= dates[1], Date <= dates[2]) %>%
      group_by(Date) %>%
      summarize(counts = n()) %>%
      tsibble(index = Date) %>%
      fill_gaps(., .full = TRUE) %>%
      tibble() %>%
      mutate(counts = replace_na(counts, 0))
    
    dfCaseManagement <- enhancedPersonLevelData %>%
      filter(Type == "Case management") %>%
      filter(Division %in% businessUnitFilter) %>%
      filter(Team %in% teamFilter) %>%
      filter(Date >= dates[1], Date <= dates[2]) %>%
      group_by(Date) %>%
      summarize(counts = n()) %>%
      tsibble(index = Date) %>%
      fill_gaps(., .full = TRUE) %>%
      tibble() %>%
      mutate(counts = replace_na(counts, 0))
    
    
    dfOtherRedeployment <- enhancedPersonLevelData %>%
      filter(Type == "Other redeployment") %>%
      filter(Division %in% businessUnitFilter) %>%
      filter(Team %in% teamFilter) %>%
      filter(Date >= dates[1], Date <= dates[2]) %>%
      group_by(Date) %>%
      summarize(counts = n()) %>%
      tsibble(index = Date) %>%
      fill_gaps(., .full = TRUE) %>%
      tibble() %>%
      mutate(counts = replace_na(counts, 0))
    
    # Validate inputs before charting
    validate(
      need(nrow(df2) >0, 'No results were found from the selected parameters.')
    )
    
  # Plot Data -------------------------------------------
    # To do - into GGPLOT and make multiple time series
    fig <- ggplotly(ggplot() +
                      {if(input$viewMultiple == 1 & sum(contacttracingFilter, contacttracingFilter, otherredploymentFilter) > 1) geom_line(data = df2, aes(x = Date, y = counts, color = CombinedColourLabel))} +
                      {if(input$contactTracing == 1) geom_line(data = dfContactTracing, aes(x = Date, y = counts, color = "Contact tracing"))} +
                      {if(casemanagementFilter == 1) geom_line(data = dfCaseManagement, aes(x = Date, y = counts, color = "Case management"))} +
                      {if(otherredploymentFilter == 1) geom_line(data = dfOtherRedeployment, aes(x = Date, y = counts, color = "Other redeployment"))} +
                      scale_colour_manual(values = c(setNames(c(rgb(0,38,100, max = 255), 
                                                                rgb(0, 171, 230, max = 255),
                                                                rgb(117,47,138, max = 255),
                                                                rgb(79,79,79, max = 255)),
                                                              c("Contact tracing",  "Case management", "Other redeployment", CombinedColourLabel)))) +
                      labs(y = "Counts") +
                      theme(panel.border = element_blank(),
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                            panel.background = element_blank(),
                            axis.line = element_line(size = 0.5, linetype = "solid",
                                                     colour = "black"),
                            legend.title=element_blank()))
    
    
  })
  
  
  output$graph2 <- renderPlotly({
    
    # TO DO - move this filtering into a single function that each output can call
    
    dates <- input$dates
    businessUnitFilter = input$selectBusinessUnit
    teamFilter = input$selectTeam
    
    contacttracingFilter = input$contactTracing
    casemanagementFilter = input$caseManagement
    otherredploymentFilter = input$otherRedeployment
    viewMultiple <- input$viewMultiple
    
    combinedtypeVectorFiltered <- fBuildTypeVectorFilter(pcontacttracingFilter = contacttracingFilter,
                                                              pcasemanagementFilter = casemanagementFilter,
                                                              potherredploymentFilter = otherredploymentFilter)
    
    
    df2 <- enhancedPersonLevelData %>%
      filter(Type %in% combinedtypeVectorFiltered) %>%
      filter(Division %in% businessUnitFilter) %>%
      filter(Team %in% teamFilter) %>%
      filter(Date >= dates[1], Date <= dates[2])
    
    
    validate(
      need(nrow(df2) >0, 'No results were found from the selected parameters')
    )
    
    # Rebuild df2 into long format
    df3 <- Server_fCreateEnhancedDataSet(df2) %>%
      mutate(TotalShiftAllocationRank = seq(n())) %>%
      pivot_longer(cols = -c(Person, TotalShiftAllocationRank), names_to = "DayGroup", values_to = "Shifts") %>%
      mutate(DayGroupLabel = case_when(DayGroup == "sundayShifts" ~ "Sunday shifts",
                                       DayGroup == "saturdayShifts" ~ "Saturday shifts",
                                       DayGroup == "weekdayShifts" ~ "Weekday shifts",
                                       DayGroup == "totalShifts" ~ "Total shifts"),
             DayGroupLabel = factor(x = DayGroupLabel, levels = c("Sunday shifts", "Saturday shifts", "Weekday shifts", "Total shifts")))
    

    
    # #TO DO - change to GGPLOT 
    fig <- ggplotly(ggplot() + geom_bar(data = df3 %>%
                                          filter(DayGroup != "totalShifts") %>%
                                          filter(TotalShiftAllocationRank <= 20),
                                        aes(x = reorder(Person, -TotalShiftAllocationRank), y = Shifts, fill = DayGroupLabel),
                                        position = "stack",
                                        stat = "identity") +
                      scale_fill_manual(values = c("Sunday shifts" = rgb(117,47,138, max = 255),
                                                     "Saturday shifts" = rgb(0,38,100, max = 255),
                                                     "Weekday shifts" = rgb(0, 171, 230, max = 255)
                      )) +
                      theme(
                            # axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
                            axis.title.y=element_blank(),
                            panel.border = element_blank(),
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                            panel.background = element_blank(),
                            axis.line = element_line(size = 0.5, linetype = "solid",
                                                     colour = "black"),
                            legend.title=element_blank()) +
                      coord_flip()) %>%
      layout(height = 600)

    
    
  })
  



  
  # Table Output ---------------------------------------
  output$shiftData = DT::renderDataTable({
    
    dates <- input$dates
    businessUnitFilter = input$selectBusinessUnit
    teamFilter = input$selectTeam
    contacttracingFilter = input$contactTracing
    casemanagementFilter = input$caseManagement
    otherredploymentFilter = input$otherRedeployment
    combinedtypeVectorFiltered <- fBuildTypeVectorFilter(pcontacttracingFilter = contacttracingFilter,
                                                              pcasemanagementFilter = casemanagementFilter,
                                                              potherredploymentFilter = otherredploymentFilter)
    
    # FILTER DATA -----------------------------------------    
    df2 <- enhancedPersonLevelData %>%
      filter(Type %in% combinedtypeVectorFiltered) %>%
      filter(Division %in% businessUnitFilter) %>%
      filter(Team %in% teamFilter) %>%
      filter(Date >= dates[1], Date <= dates[2]) 
    
  
    
    
    datatable(
         df2, 
        extensions = 'Buttons', options = list(
          dom = 'Bfrtip',
          buttons = c('pageLength', 'copy', 'csv', 'excel', 'pdf', 'print')
      )
    )
  })
   
  
  output$enhancedDataTable = DT::renderDataTable({
    
    
    
    dates <- input$dates
    businessUnitFilter = input$selectBusinessUnit
    teamFilter = input$selectTeam
    contacttracingFilter = input$contactTracing
    casemanagementFilter = input$caseManagement
    otherredploymentFilter = input$otherRedeployment
    combinedtypeVectorFiltered <- fBuildTypeVectorFilter(pcontacttracingFilter = contacttracingFilter,
                                                              pcasemanagementFilter = casemanagementFilter,
                                                              potherredploymentFilter = otherredploymentFilter)
    
    df2 <- enhancedPersonLevelData %>%
      filter(Type %in% combinedtypeVectorFiltered) %>%
      filter(Division %in% businessUnitFilter) %>%
      filter(Team %in% teamFilter) %>%
      filter(Date >= dates[1], Date <= dates[2])
    

    
    df3 <- Server_fCreateEnhancedDataSet(df2) %>%
      rename(`Sunday shifts` = sundayShifts,
             `Saturday shifts` = saturdayShifts,
             `Weekday shifts` = weekdayShifts,
             `Total shifts` = totalShifts)
    
    df4 <-  merge(df3, staffData  , by = "Person",  all.x = TRUE)


    datatable(
       df4,
      extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('pageLength', 'csv', 'excel', 'pdf', 'print')
      )
    )
  })
  
  
  output$blurb <- renderText({
    
    dates <- input$dates
    businessUnitFilter = input$selectBusinessUnit
    teamFilter = input$selectTeam


    
    blurbText <- paste0("Between ", 
                        months(dates[1]), " ", day(dates[1]), ", ",
                        year(dates[1]), 
                        " and ", 
                        months(dates[2]), " ", day(dates[2]), ", ",
                        year(dates[2]), 
                        " the Cancer Institute will provide support for ",
                         " 1023 ", 
                        " contact tracing shifts and ",
                        "12 ",
                        " case management shifts. ",
                        "During this time period, the greatest impact on existing programs of work will be experienced in the  ",
                        "Primary Care, Registries, and ",
                        "QSP ",
                        "Teams.")
    return(blurbText)
    
    
  })
  
  # About output
  output$AboutText <- renderText({
    fGetSettings()$AboutText
  })
  
  
  output$aggData <- downloadHandler(
    filename = function() { "CancerInstituteCovidResponse.xlsx"},
    content = function(file) {
      

      
      dates <- input$dates
      businessUnitFilter = input$selectBusinessUnit
      teamFilter = input$selectTeam
      contacttracingFilter = input$contactTracing
      casemanagementFilter = input$caseManagement
      otherredploymentFilter = input$otherRedeployment
      combinedtypeVectorFiltered <- fBuildTypeVectorFilter(pcontacttracingFilter = contacttracingFilter,
                                                           pcasemanagementFilter = casemanagementFilter,
                                                           potherredploymentFilter = otherredploymentFilter)
      
      
      df2 <- enhancedPersonLevelData %>%
        filter(Type %in% combinedtypeVectorFiltered) %>%
        filter(Division %in% businessUnitFilter) %>%
        filter(Team %in% teamFilter) %>%
        filter(Date >= dates[1], Date <= dates[2])
      
      
      
      df3 <- Server_fCreateEnhancedDataSet(df2) %>%
        rename(`Sunday shifts` = sundayShifts,
               `Saturday shifts` = saturdayShifts,
               `Weekday shifts` = weekdayShifts,
               `Total shifts` = totalShifts)
      
      df4 <-  merge(df3, staffData  , by = "Person",  all.x = TRUE)
      
      
      # FILTER DATA -----------------------------------------    
      df5 <- enhancedPersonLevelData %>%
        filter(Type %in% combinedtypeVectorFiltered) %>%
        filter(Division %in% businessUnitFilter) %>%
        filter(Team %in% teamFilter) %>%
        filter(Date >= dates[1], Date <= dates[2]) 
      
      
      
      
      write_xlsx(list(`Individual shift data` = df5, `Aggregate shift data` = df4), path = file)}
  )
  

 
}
#Run App
shinyApp(ui, server)



