library(tidyverse)
library(lubridate)
library(httr)
library(dplyr)
library(xml2)



#build the url and GET the result
url <- "https://oeis.org/A005408"
h <- handle(url)
res <- GET(handle = h)
a<- read_html(res)
htmlCode <- xml_find_all(a, ".//body")


# create empty frame
df <- data.frame(header = character(0),
                 pText = character(0))


# get all td tags
td <- xml_find_all(htmlCode, ".//td")



# Iterate through all td tags
for (x in td) {
  
  if (length(xml_find_first(x, ".//font")) > 0) {
    
    section <- xml_text(xml_find_first(x, ".//font"))
    print(section)
  }
  

  # if there are p tags in this cell
  if(length(xml_find_all(x, ".//p")) > 0) {
    
    
    pTags <- xml_find_all(x, ".//p")

    
    for (p in pTags) {
      print(paste0("Section to add in DataFrame -->", section))
      print(paste0("PTag to add -->", as.character(p)))
      
      dataRow <- data.frame(
        header = section,
         pText = as.character(p)
         
         )
        df <- rbind(df, dataRow)
      
    }
    
    print("END OF ITERATING THROUGH PTAGS")

    #print(xml_text(pTags))
  }
  

}

glimpse(df)

