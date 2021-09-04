fGetSettings <- function() {
  
  settings <- list() 
  
  settings$Version <- "0.1"
  settings$AuthorNames <- "Jamie Gabriel"
  
  
  settings$AboutText <- "This app has been created to manage the Cancer Institute Covid Response. It uses data taken 
  from the MS Teams Covid 19 site and is refreshed approximately every 24 hours.
  
  <br/><br/>
  <b><u>Data Sources</b></U>:<br/>
  
1.<br/>  <b>MS Teams Site</b>: https://nswhealth.sharepoint.com/sites/PHEOCCancerInstituteMOH-MoH<br/>
  <b>Document</b><i>General/CINSW Roster - PLEASE DO NOT MAKE COPIES.xlsx</i>
  <br/><br/>

2. <br/><b>MS Teams Site</b>:  https://nswhealth.sharepoint.com/sites/COVID-19-CloseContactFollowUp-CINSW-TEAMLEADS'<br/>
  <b>Document</b><i>TEAM LEADS/CINSW Close Contact Tracing 2021 - Reporting.xlsx</i>


  
  <br/><br/>
  If you have any question/feedback please send to Jamie.Gabriel@health.nsw.gov.au <br/><br/>
  
  The code for this app is held at <a href='https://cinsw.visualstudio.com/Shiny%20Apps/_git/ShinyAppCovidResponse'>https://cinsw.visualstudio.com/Shiny%20Apps/_git/ShinyAppCovidResponse</a>
  
  "
  
  
  settings$PathToData <- "./Data"
    
  return(settings)
  
}