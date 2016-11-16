# Read in and light munging on the Raleigh inspection data. 

library(data.table) 
library(readr)
library(magrittr)
library(lubridate)
library(sp)
library(geosphere)

 
#  Read in data. ----------------------------------------------------------
# Using readr::read_csv seems better on these data (fread has errors). 
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

# Violation codes ---------------------------------------------------------
# Quick insight into violation codes. Because there are so many codes, it doesn't
# make much sense to keep them around, such as in a wide format (and the 
# long format is inconvenient because it is not one inspection per row).  
violation[ , unique(ViolationCode)]  # show all codes , ~ 300 of them.
violation[ , .N, by = ViolationCode][order(-N)] %>% 
  write_csv(path = "raleigh/data/violation-freq.csv")  # write out frequencies

# Each violation code can take on different levels of `critical`, suggesting
# it is not the code that is critical or not, but how severely code was violated.
violation[ , .N, by = .(critical, ViolationCode)][order(ViolationCode)]

# Aggregate violations to the inpsection date. 
violation_agg <- violation[ , .(num_critical = length(which(critical == "Yes")), 
               num_non_critical = length(which(critical == "No" | is.na(critical)))),
           by = .(HSISID, InspectDate, InspectedBy)]


# Merge inspections and violations.  --------------------------------------
dat <- merge(inspection, violation_agg, by.x = c("HSISID", "Date"), 
             by.y = c("HSISID", "InspectDate"), all.x = TRUE)  # left join
# We note that in `violation_agg`, there are no NA `num_critical` or 
# `num_non_critical`. So if there are NA values for these columns in dat, 
# that means there was no violation that matched that inspection. However, 
# it does not appear they had no violations, as scores are not that different
# from ones that did match. 
dat[is.na(num_critical), mean(Score)]
dat[!is.na(num_critical), mean(Score)]
# Because of above, we remove any inspections that do not have violation info. 
# Note: this is equivalent to doing left-join above. 
dat <- subset(dat, !is.na(num_critical) & !is.na(num_non_critical))
all.equal(dat, merge(inspection, violation_agg, by.x = c("HSISID", "Date"), 
                by.y = c("HSISID", "InspectDate")))  # equivalent to left-join


# Merge insections and violations with restaurant info.  ------------------
# Examine mis-matches first: those in inspection, not in restaurant_info. 
dat[!(HSISID %in% restaurant_info$HSISID), ]
dat <- merge(dat, restaurant_info, by = "HSISID")  # perform merge (inner join)

# Remove scores of zero. 
dat <- subset(dat, Score != 0)

# Normalize the city strings. 
dat[ , City := tolower(City)]
dat[City == "morrisvile", City := "morrisville"]
dat[City == "holly spring", City := "holly springs"]
dat[City == "fuquay varina", City := "fuquay-varina"]

# Create a 5-digit zip code. 
dat[ , zip := substr(PostalCode, 1, 5)]


# Compute previous violations.  -------------------------------------------
dat <- dat[order(HSISID, Date)]
dat[ , num_critical_previous := shift(num_critical, 1, type = "lag"), by = HSISID]

# Compute previous inspections info. --------------------------------------
dat <- dat[order(HSISID, Date)]
dat[ , previous_inspection_date := shift(Date, 1, type = "lag"), by = HSISID]
dat[ , days_since_previous_inspection := 
           as.numeric(difftime(Date, previous_inspection_date, units = "days"))]
dat[ , days_from_open_date := 
       as.numeric(difftime(Date, RestaurantOpenDate, units = "days"))]


# Compute nearest neighbors' critical violations. ------------------------------
dat <- subset(dat, !(X == 0 | Y == 0))  # ensure we have lat/long
dat[ , c("avg_neighbor_num_critical", "avg_neighbor_num_non_critical") := -1]
all_location <- as.matrix(dat[ , list(X, Y)])
dat_loc <- subset(dat, select = c("HSISID", "Date", 
                                  "num_critical", "num_non_critical", 
                                  "X", "Y"))
n <- nrow(dat)
for (i in 1:n){
  print(paste("Proccesing", i, "of", n))
  curr_record <- dat[i,]
  curr_loc <- c(curr_record$X, curr_record$Y)
  curr_date <- curr_record$Date
  curr_id <- curr_record$HSISID
  dat_loc$dist <- distGeo(curr_loc, as.matrix(dat_loc[ , list(X, Y)]))
  res <- dat_loc[Date < curr_date & HSISID != curr_id][order(dist)][ 
    , .(mean(num_critical), mean(num_non_critical)), by = HSISID][1:5, ][ ,
      .(neigh_crit = mean(V1), neigh_non_crit = mean(V2), 
        top_match = HSISID[1], second_match = HSISID[2])]
  dat[HSISID == curr_id & Date == curr_date, 
      `:=`(avg_neighbor_num_critical = res$neigh_crit, 
           avg_neighbor_num_non_critical = res$neigh_non_crit, 
           top_match = res$top_match, 
           second_match = res$second_match)]
}

# Write out the data. 
write_csv(dat, path = "raleigh/data/inspections.csv")
