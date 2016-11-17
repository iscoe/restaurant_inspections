# Merge the Raleigh restaurant inspections/violations with Census and Yelp data.

library(data.table) 
library(magrittr)
library(tidyr)

# Read in data. -----------------------------------------------------------
inspections <- read_csv("raleigh/data/inspections.csv") %>% data.table()
census <- read_csv("raleigh/data/census/census.csv") %>% data.table()
# TODO: read in Yelp data. 


# Merge inspections and census by zip.  -----------------------------------
# Examine mis-matches. 
setdiff(census$zip, inspections$zip)
setdiff(inspections$zip, census$zip)

# Even though there are number of zip code mis-matches, they do not affect
# many records. 
inspections[zip %in% setdiff(inspections$zip, census$zip), .N]
nrow(inspections)

# Inner join. 
inspections_census <- merge(inspections, census, by = "zip")


# Merge in Yelp data. -----------------------------------------------------
# TODO. 


# Write out data.  --------------------------------------------------------
write_csv(inspections_census, "raleigh/data/merged.csv")
