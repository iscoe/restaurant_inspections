# Read in and light munging on Denver data. 

library(data.table) 
library(readxl)
library(magrittr)
library(stringr)
library(lubridate)

# Read in inspections, enforcement action, establishment geocoded data.  
inspection <- fread("denver/data/Inspections.csv")
enforce_action <- fread("denver/data/DenverGOV_EnforcementAction.csv")
establish_geo <- read_excel("denver/data/DenverGOV_Establishments_Geocoded.xls") %>% 
  data.table()

# Light munging on inspections. 
new_cols <- gsub(" ", "_", names(inspection)) %>% tolower()
setnames(inspection, new_cols)
inspection <- unique(inspection)  # remove duplicate rows

# Light munging on Enforcement Action. 
enforce_action[ , c("V11", "Row Number") := NULL]
setnames(enforce_action, names(enforce_action)[1], "Restaurant")
new_cols <- gsub(" ", "_", names(enforce_action)) %>% tolower()
setnames(enforce_action, new_cols)
enforce_action[ , restaurant := toupper(gsub(" ", "", restaurant))]
enforce_action[ , inspection_date := mdy_hms(inspection_date)]


# Light munging on Establishment Geocoded. 
new_cols <- gsub(" ", "_", names(establish_geo)) %>% tolower()
setnames(establish_geo, new_cols)
establish_geo[ , restaurant := toupper(gsub(" ", "", restaurant))]

# Examine how well restaurants match up. 
intersect(enforce_action$restaurant, establish_geo$restaurant) %>% head()
setdiff(enforce_action$restaurant, establish_geo$restaurant) %>% head()
setdiff(establish_geo$restaurant, enforce_action$restaurant) %>% head()
establish_geo[ , uniqueN(restaurant)]
enforce_action[ , uniqueN(restaurant)]
merge(enforce_action, establish_geo, by = "restaurant")

x <- setdiff(enforce_action$restaurant, establish_geo$restaurant) %>% as.character()
# View(data.frame(u = x))  # need to do this to view for some reason (RStudio error)

# Hack-ish merge: we have NOT resolved a lot of the restaurants that fail to match up. 
dat <- merge(enforce_action, establish_geo, by = "restaurant")



