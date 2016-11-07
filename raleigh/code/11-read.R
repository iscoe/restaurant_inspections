# Read in and light munging on the Raleigh data. 

library(data.table) 
library(readr)
library(magrittr)
library(lubridate)

# Read in data. Using readr::read_csv seems better on these data (fread has errors). 
inspection <- read_csv("raleigh/data/Food_Inspections.csv") %>% 
  data.table()
violation <- read_csv("raleigh/data/Food_Inspection_Violations.csv") %>% 
  data.table()
restaurant_info <- read_csv("raleigh/data/Restaurants_in_Wake_County.csv") %>% 
  data.table()

# Light munging. 
# Remove first column which is OBJECTID, but it has some kind of byte order mark
# so removing it by number. 
inspection <- subset(inspection, select = -1)
violation <- subset(violation, select = -1)
restaurant_info <- subset(restaurant_info, select = -1)

# Examine how well the datasets would merge.
inspection[ , uniqueN(HSISID)]
violation[ , uniqueN(HSISID)]
restaurant_info[ , uniqueN(HSISID)]
setdiff(inspection$HSISID, violation$HSISID)
setdiff(violation$HSISID, inspection$HSISID)
setdiff(inspection$HSISID, restaurant_info$HSISID) %>% length()  # > 1,000 mis-matches
setdiff(restaurant_info$HSISID, inspection$HSISID) %>% length()


# Merge. 
dat <- merge(inspection, violation, by.x = c("HSISID", "Date"), 
             by.y = c("HSISID", "InspectDate"), all = TRUE)  # full outer join
# Examine further the mis-matches: those that are in inspection, not in restaurant_info. 
dat[!(HSISID %in% restaurant_info$HSISID), ] %>% View

# We note that in `violation`, there are no ViolationCode's that are NA. So in 
# the merged dataset, if the ViolationCode is NA, it is because there was no 
# matching violation, so we set the value of the ViolationCode to "none", i.e., 
# there was an inspection with no violation. 
violation[is.na(ViolationCode)]  # empty
violation[ , unique(ViolationCode)]  # show all codes 
dat[is.na(ViolationCode) , ViolationCode := "none"]
dat <- merge(dat, restaurant_info, by = "HSISID")

