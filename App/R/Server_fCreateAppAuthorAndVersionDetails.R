fCreateAppAuthorAndVersionDetails <- function(pSettings) {

  return(
    renderText({
      paste0("Created by ", 
             pSettings$AuthorNames,
             " (v  ",
             pSettings$Version,
             ")")
    })
  )
  
    
}