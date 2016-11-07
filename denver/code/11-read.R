library(data.table) 
library(readxl)
library(magrittr)
library(stringr)
library(lubridate)

# Read in enforcement action data. 
enforce_action <- fread("denver/data/DenverGOV_EnforcementAction.csv")
establish_geo <- read_excel("denver/data/DenverGOV_Establishments_Geocoded.xls") %>% 
  data.table()

# Light munging on Enforcement Action. 
enforce_action[ , c("V11", "Row Number") := NULL]
setnames(enforce_action, names(enforce_action)[1], "Restaurant")
new_cols <- gsub(" ", "_", names(enforce_action)) %>% tolower()
setnames(enforce_action, new_cols)
enforce_action[ , restaurant := str_trim(restaurant)]
enforce_action[ , inspection_date := mdy_hms(inspection_date)]


# Light munging on Establishment Geocoded. 
new_cols <- gsub(" ", "_", names(establish_geo)) %>% tolower()
setnames(establish_geo, new_cols)



