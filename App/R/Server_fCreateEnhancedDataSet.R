Server_fCreateEnhancedDataSet <- function(df) {
  
  

  
  staffProfileData <- df
  
  
  df2 <- df %>%
    mutate(day = wday(Date, label = TRUE)) %>%
    mutate(isWeekend = case_when(day == "Sat" ~ "YES",
                                 day == "Sun" ~ "YES",
                                 TRUE ~ "NO"))
  

  
  df3 <- df2 %>% 
    dplyr::filter(isWeekend == "NO") %>% 
    dplyr::group_by(Person) %>%
    summarise(weekdayShifts = n())

  
  df4 <- df2 %>% 
    dplyr::filter(day == "Sat") %>% 
    dplyr::group_by(Person) %>%
    summarise(saturdayShifts = n())
  
  df5 <- df2 %>% 
    dplyr::filter(day == "Sun") %>% 
    dplyr::group_by(Person) %>%
    summarise(sundayShifts = n())
  

  
  df6 <- merge(df3, df4, by = "Person",  all = TRUE)
  df7 <- merge(df5, df6, by = "Person",  all = TRUE)
  
  

  
  df8 <- df7 %>%
    mutate(weekdayShifts = replace_na(weekdayShifts, 0)) %>%
    mutate(saturdayShifts = replace_na(saturdayShifts, 0)) %>%
    mutate(sundayShifts = replace_na(sundayShifts, 0)) %>%
    mutate(totalShifts = saturdayShifts + sundayShifts + weekdayShifts) %>%
    arrange(desc(totalShifts))
  
 # df9 <- merge(df8, staffProfileData, by = "Person", all.x = TRUE)
  
  
  return(df8)
  
}