library(Microsoft365R)
library(readxl)
library(tidyverse)
library(lubridate)


###OVERVIEW




### GET DATA FROM MS TEAMS ##################################################################################


PHEOCConnection <- get_sharepoint_site(site_url = 'https://nswhealth.sharepoint.com/sites/PHEOCCancerInstituteMOH-MoH')
PHEOCSharePointDrive  <- PHEOCConnection$get_drive("Documents")
PHEOCSharePointDrive$download_file("General/CINSW Roster - as at 23 Aug 2021.xlsx", "./Data/ContractTracingShiftData.xlsx", overwrite = TRUE)


CICovidResponseConnection <- get_sharepoint_site(site_url = 'https://nswhealth.sharepoint.com/sites/COVID-19-CloseContactFollowUp-CINSW-TEAMLEADS')
CICovidResponseSharePointDrive <- CICovidResponseConnection$get_drive("Documents")

CICovidResponseSharePointDrive$download_file("TEAM LEADS/CINSW CCT Support Tracking - 2021.xlsx", "./Data/StaffProfileData.xlsx", overwrite = TRUE)


CICovidCMResponseConnection <- get_sharepoint_site(site_url = 'https://nswhealth.sharepoint.com/sites/COVID-19-CloseContactFollowUp-CINSW')
CICovidCMResponseSharePointDrive <- CICovidCMResponseConnection$get_drive("Documents")
CICovidCMResponseSharePointDrive$download_file("COVID-19 Response - 30 July 2021/Case Management Roster - CINSW only.xlsx", "./Data/CaseManagementShiftData.xlsx", overwrite = TRUE)





############################################################################################################
# PREPARE PHEOCC ROSTER DATA ###############################################################################
############################################################################################################

df0 <- readxl::read_excel("./Data/ContractTracingShiftData.xlsx", sheet = "ROSTER", col_types = "text", skip = 0)



df0 <- df0 %>%
  rename(Name = "...2",
         Email = "...1")

# REMOVE UNNEEDED COLUMNS AND PIVOT LONGER TO GET ALL INSTANCES OF SHIFTS #
df1 <- df0 %>% 
  select(-`Email`, -`...3`) 

df2 <- df1 %>%
  pivot_longer(!`Name`, names_to = "shiftDates", values_to = "shiftUndertaken")  %>% 
  mutate(shiftUndertaken = trimws(str_to_sentence(shiftUndertaken))) %>%
  filter(complete.cases(.)) %>%
  filter(shiftUndertaken == "Yes") %>%
  mutate(shiftDates = as.Date(as.numeric(shiftDates),  origin = "1899-12-30")) %>%
  select(-shiftUndertaken) %>%
  rename(Person = Name, Date = shiftDates) %>%
  mutate(Person = trimws(str_to_title(Person)))



#  WRITE CLEANED DATA TO APP DATA STORE #
#




############################################################################################################
# PREPARE STAFF PROFILE DATA ###############################################################################
############################################################################################################



df4 <- readxl::read_excel("./Data/ContactTracingStaffData2.xlsx" , sheet = "ONBOARDED ")

df5 <- df4 %>%
  filter(NAME != "TEAM LEADS") %>%
  filter(NAME != "CALLERS") %>%
  filter(NAME != "CASE MANAGERS") %>%
  select(-Caller, -Both, -CM) %>%
  mutate(NAME = trimws(str_to_title(NAME)))


df6 <- df5 %>%
  filter(complete.cases(.)) %>%
  select(Person = NAME, Division = DIVISION, Team = TEAM)




# CASE MANAGEMETN ####################################

df7 <- readxl::read_excel("./Data/CaseManagementShiftData.xlsx",col_types = "text", skip = 0)



colNames1 <- as.character(df7[2:2, 1:9])
colNames2 <- as.character(df7[1:1, 10: ncol(df7)])
allColNames <- c(colNames1, colNames2)
colnames(df7) = allColNames
df8 <-tail(df7, -2)


df9 <- df8 %>%
  dplyr::select(`First Name`,
                starts_with("44"))



df10 <- df9 %>%
  pivot_longer(!`First Name`, names_to = "shiftDates", values_to = "shiftUndertaken")  %>% 
  filter(complete.cases(.)) %>%
  filter(shiftUndertaken == "Yes") %>%
  mutate(shiftDates = as.Date(as.numeric(shiftDates),  origin = "1899-12-30")) %>%
  select(-shiftUndertaken) %>%
  rename(Person = `First Name`, Date = shiftDates) %>%
  mutate(Type = "Case management",
         Person = trimws(str_to_title(Person)))



## ADD HISTORICAL DATA #######################################################################################

df11 <- readxl::read_excel("./Data/HistoricalData/AllHistoricalData.xlsx")
df12 <- rbind(df2, df11)
df13 <- df12 %>%
  mutate(Type = "Contact tracing")
df14 <- rbind(df13, df10)




## ADD OTHER REDEPLOYMENT DATA ##########################################
df15 <- readxl::read_excel("./Data/RawOtherRedploymentData.xlsx", skip = 1)
df16 <- df15 %>%
  select(`FIRST NAME`, `SURNAME`, `Start Date`, `End Date`) %>%
  filter(complete.cases(.)) %>%
  mutate(Person = paste0(`FIRST NAME`, " ", SURNAME),
         Person = trimws(str_to_title(Person))) %>%
  select(-`FIRST NAME`, -`SURNAME`) %>%
  rename(start = `Start Date`, end = `End Date`)


df17 <- data.frame(matrix(ncol = 2, nrow = 0))
df17Cols <- c("Person", "Date")
colnames(df17) <- df17Cols


for(i in 1:nrow(df16)) {       # for-loop over columns
  
  startDate = df16[i, 1][[1]]
  endDate = df16[i, 2][[1]]
  person = df16[i, 3][[1]]
  numberOfDaysBetweenDates <- interval(startDate,endDate)/days(1)
  
  
  listOfDatesBetweenStartAndEnd = startDate + days(0:numberOfDaysBetweenDates)
  
  df <- data.frame(listOfDatesBetweenStartAndEnd)
  
  df$Person = person
  
  colnames(df) <- c("Date", "Person")
  
  df <- df %>% dplyr::select("Person", "Date")
  
  df17 <- rbind(df17, df)
  
  
}

df18 <- df17 %>%
  dplyr::mutate(Type = "Other redeployment" )


###






df19 <- rbind(df14, df18)

# WRITE DATA (COPY TO DATA PREP, COPY TO APP ##########################
write_csv(df19, paste0("../DataPrep/Data/cleanedShiftData", Sys.Date(), ".csv"))
write_csv(df19, "../App/Data/cleanedContractTracingShiftData.csv")


df20 <- df16 %>%
  dplyr::select(Person) %>%
  dplyr::mutate(Division = "Not provided", Team = "Not provided") 

df21 <- rbind(df6, df20)


# 
write_csv(df21, paste0("../DataPrep/Data/cleanedStaffData", Sys.Date(), ".csv"))
write_csv(df21, "../App/Data/cleanedContactTracingStaffData.csv")

