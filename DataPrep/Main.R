library(tidyverse)
library(lubridate)
library(httr)
library(XML)
library(dplyr)
library(xml2)


###OVERVIEW

#build the url and GET the result
url <- "https://oeis.org/A005408"
h <- handle(url)
res <- GET(handle = h)

a<- read_html(res)

htmlCode <- xml_find_all(a, ".//body")






sectGive <- function(node) {
  # MAYBE AS FUCNTION
}



df <- data.frame(header = character(0),
                 pText = character(0))

td <- xml_find_all(htmlCode, ".//td")


for (x in td) {
  

  if (length(xml_find_first(x, ".//font")) > 0) {
    section <- xml_text(xml_find_first(x, ".//font"))
    print(section)
    
  } else {
    section <- "Not defined"
  }
  
  if(length(xml_find_all(x, ".//p")) > 0) {
    pTags <- xml_find_all(x, ".//p")

    
    print(section)
    
    for (p in pTags) {
      
      dataRow <- data.frame(
        header = section,
         pText = xml_text(p)
         
         )
        df <- rbind(df, dataRow)
      
    }
    
    print("END")
    #print(xml_text(pTags))
  }
  
  # if (xml_length(x) > 5) {
  #   print(xml_text(x, trim = TRUE))
  # }
}

glimpse(df)
