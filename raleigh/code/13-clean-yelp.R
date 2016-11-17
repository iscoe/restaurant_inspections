# Read in and clean up Yelp data. 

library(data.table) 
library(readr)
library(magrittr)
library(lubridate)
library(stringi)


# Read in data. -----------------------------------------------------------

yelp <- read_csv("raleigh/data/raleighYelpData.csv") %>% data.table()
categories <- read_csv("raleigh/data/raleighYelpNoNullCategories.csv") %>% 
  data.table()

yelp[ , V1 := NULL]        # delete row id
categories[ , V1 := NULL]  # delete row id

# Compute mean of each category. 
cats <- setdiff(names(categories), "bus_id")
top_20_cats <- categories[ , lapply(.SD, mean, na.rm = TRUE), .SDcols = cats] %>% 
  sort() %>% rev() %>% t() %>% head(20) %>% rownames()

categories <- subset(categories, select = c("bus_id", top_20_cats))

# Merge the Yelp categories with the Yelp reviews. 
setdiff(categories$bus_id, yelp$id)
setdiff(yelp$id, categories$bus_id)
yelp <- merge(yelp, categories, by.x = "id", by.y = "bus_id")

# Take unique based on phone number. 
setkeyv(yelp, c("phone"))
yelp <- unique(yelp)

# Categories as a single comma-separated string no longer needed, we have the 
# binary variables now. 
yelp[ , categories := NULL]

# Convert name to lower. 
yelp[ , name := stri_trans_tolower(name)]

# Write out data.  --------------------------------------------------------
write_csv(yelp, "raleigh/data/yelp.csv")
