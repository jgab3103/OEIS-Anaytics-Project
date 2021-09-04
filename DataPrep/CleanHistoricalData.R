library(Microsoft365R)
library(readxl)
library(tidyverse)
library(lubridate)


fCleanHistoricalData <- function(df0) {
  
  
  excelColNames <- colnames(df0)
  updatedColNames <-  as.Date(as.numeric(excelColNames),  origin = "1899-12-30")
  
  colnames(df0) <- updatedColNames

  
  # Create data frame
  mergedRoster <- data.frame(matrix(ncol = 2, nrow = 0))
  n <- c("Person", "Date")
  colnames(mergedRoster) <- n
  
  
  for(i in 1:ncol(df0)) {       # for-loop over columns
    
    singleDay <- df0[, i,i]
    print(singleDay)
    df <- data.frame(singleDay)
    df$Date = colnames(df0)[i]
    colnames(df) <- c("Person", "Date")
    df <- df %>% drop_na()
    # 
    mergedRoster <- rbind(mergedRoster, df)
    
    
  }
  
  return(mergedRoster)
  
}


df0 <- readxl::read_excel("./Data/HistoricalData/RosterJuly2020.xlsx")
df1 <- readxl::read_excel("./Data/HistoricalData/RosterJuneV2.xlsx")
julyData <- fCleanHistoricalData(df0)
juneData <- fCleanHistoricalData(df1)


allHistoricalData <- rbind(julyData, juneData)

write_csv(allHistoricalData, "./Data/HistoricalData/AllHistoricalData.")

writexl::write_xlsx(allHistoricalData, "./Data/HistoricalData/AllHistoricalData.xlsx")












