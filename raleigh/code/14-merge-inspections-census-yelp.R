# Merge the Raleigh restaurant inspections/violations with Census and Yelp data.

library(data.table) 
library(magrittr)
library(tidyr)
library(stringi)
library(stringdist)
library(readr)

# Read in data. -----------------------------------------------------------
inspections <- read_csv("raleigh/data/inspections.csv") %>% data.table()
census <- read_csv("raleigh/data/census/census.csv") %>% data.table()
yelp <- read_csv("raleigh/data/yelp.csv") %>% data.table()


# Merge inspections and census by zip.  -----------------------------------
inspections[ , .(N = .N, unique_HSISID = uniqueN(HSISID))]

# Examine mis-matches. 
setdiff(census$zip, inspections$zip)
setdiff(inspections$zip, census$zip)

# Even though there are number of zip code mis-matches, they do not affect
# many records. 
inspections[zip %in% setdiff(inspections$zip, census$zip), .N]
nrow(inspections)

# Inner join. 
inspections_census <- merge(inspections, census, by = "zip")
inspections_census[ , .(N = .N, unique_HSISID = uniqueN(HSISID))]

# Merge in Yelp data. -----------------------------------------------------
# Clean up names. 
inspections_census[ , Name := stri_trans_tolower(Name)]
inspections_census[ , Name := gsub("`|&|\\.|-|'", "", Name)]
inspections_census[ , Name := gsub("", "", Name)]
inspections_census[ , Name := gsub("amp;", "", Name)]
inspections_census[ , Name := gsub("  ", "", Name)]
yelp[ , name := gsub("'", "", name)]
yelp[ , name := gsub("amp;", "", name)]
yelp[ , name := gsub("  ", "", name)]

# Clean phone numbers. 
yelp[ , phone := as.character(phone)]
inspections_census[ , PhoneNumber := gsub("\\(|\\)| |-", "", PhoneNumber)]
inspections_census[ , PhoneNumber := paste0("1", substr(PhoneNumber, 1, 10))]

# Approximately match names. 
# yelp$name_match <- amatch(yelp$name, inspections_census$Name, maxDist = 1)
# yelp$name_match_inspect <- inspections_census$Name[yelp$name_match]
# yelp$phone_match_inspect <- inspections_census$PhoneNumber[yelp$name_match]
# yelp$address_match_inspect <- inspections_census$Address1[yelp$name_match]
# subset(yelp, name != name_match_inspect & phone != phone_match_inspect, 
#        select = c("name", "phone", "location.address1", 
#                   "name_match_inspect", "phone_match_inspect", "address_match_inspect")) %>% View


setdiff(inspections_census$PhoneNumber, yelp$phone)

# Match by exact phone number. 
inspections_census_yelp <- merge(inspections_census, yelp, 
                            by.x = "PhoneNumber", by.y = "phone", all.x = TRUE)
inspections_census_yelp[ , .(N = .N, unique_HSISID = uniqueN(HSISID))]
# Use the Yelp ID to see how many there are with Yelp data. 
inspections_census_yelp[!is.na(id) , .(N = .N, unique_HSISID = uniqueN(HSISID))]

# Write out data.  --------------------------------------------------------
write_csv(inspections_census_yelp, "raleigh/data/merged.csv")
