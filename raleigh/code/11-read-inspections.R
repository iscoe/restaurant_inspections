# Read in and light munging on the Raleigh inspection data. 

library(data.table) 
library(readr)
library(magrittr)
library(lubridate)
library(sp)
library(geosphere)
library(knitr)

 
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

# Remove Re-Inspections. 
print("Removing re-inspections:")
inspection[ , .N, by = Type]
inspection[ , .N / nrow(inspection), by = Type]  # re-inspections approx. 1.4% of data
inspection <- subset(inspection, Type != "Re-Inspection")

# Look at date range. 
inspection[ , min(Date)]
inspection[ , max(Date)]
violation[ , min(InspectDate)]
violation[ , max(InspectDate)]

# Examine how well the datasets would merge.
inspection[ , .(N = .N, unique_HSISID = uniqueN(HSISID))]
violation[ , .(N = .N, unique_HSISID = uniqueN(HSISID))]
restaurant_info[ , .(N = .N, unique_HSISID = uniqueN(HSISID))]
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

top_codes <- violation[ , .N, by = ViolationCode][order(-N)][1:10, ViolationCode]
hasViolation <- function(code, violationVec){ any(violationVec == code) }
top_codes_freq <- violation[ , lapply(top_codes, hasViolation, ViolationCode), 
           by = .(HSISID, InspectDate)]
setnames(top_codes_freq, paste0("V", 1:10), top_codes)
top_codes_df <- top_codes_freq[ , lapply(.SD, mean), .SDcols = top_codes] %>% 
  t() %>% 
  data.frame()
colnames(top_codes_df) <- "Frequency"
top_codes_df$code <- rownames(top_codes_df)
top_codes_df <- subset(top_codes_df, select = c("code", "Frequency"))  # re-order columns
write_csv(top_codes_df, 
          path = "raleigh/data/violation-freq-top-codes.csv")  # write out frequencies
kable(top_codes_df, digits = 3, row.names = FALSE)

# Each violation code can take on different levels of `critical`, suggesting
# it is not the code that is critical or not, but how severely code was violated.
violation[ , .N, by = .(critical, ViolationCode)][order(ViolationCode)]

# Aggregate violations to the inpsection date. 
violation_agg <- violation[ , .(num_critical = length(which(critical == "Yes")), 
               num_non_critical = length(which(critical == "No" | is.na(critical)))),
           by = .(HSISID, InspectDate, InspectedBy)]
violation_agg[ , .(N = .N, unique_HSISID = uniqueN(HSISID))]


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
dat <- dat[order(HSISID, Date)]
dat[ , num_non_critical_previous := shift(num_non_critical, 1, type = "lag"), by = HSISID]

# Compute rolling mean of previous violations. -------------------------------
# Critical violations. 
dat <- dat[order(HSISID, Date)]
dat[ , `:=`(cum_sum = cumsum(num_critical), index = 1:.N), by = HSISID]
dat[ , cum_sum_minus_current := cum_sum - num_critical]  # exclude the current from cumsum
dat[index != 1, num_critical_mean_previous := cum_sum_minus_current / (index - 1)]
dat[ , c("cum_sum", "cum_sum_minus_current", "index") := NULL]
# Non-critical violations. 
dat <- dat[order(HSISID, Date)]
dat[ , `:=`(cum_sum = cumsum(num_non_critical), index = 1:.N), by = HSISID]
dat[ , cum_sum_minus_current := cum_sum - num_non_critical]  # exclude the current from cumsum
dat[index != 1, num_non_critical_mean_previous := cum_sum_minus_current / (index - 1)]
dat[ , c("cum_sum", "cum_sum_minus_current", "index") := NULL]


# Compute previous inspections info. --------------------------------------
dat <- dat[order(HSISID, Date)]
dat[ , previous_inspection_date := shift(Date, 1, type = "lag"), by = HSISID]
dat[ , days_since_previous_inspection := 
           as.numeric(difftime(Date, previous_inspection_date, units = "days"))]
dat[ , days_from_open_date := 
       as.numeric(difftime(Date, RestaurantOpenDate, units = "days"))]


# Inspection number and inspector ID.  ------------------------------------
dat <- dat[order(Date, HSISID)]
dat[ , inspection_num := 1:.N, by = HSISID]
dat <- dat[order(Date, HSISID)]
dat[ , inspector_ID := as.numeric(factor(InspectedBy))]
dat[ , previous_inspection_by_same_inspector := c(0, diff(inspector_ID)) == 0, 
     by = HSISID]
dat[inspection_num == 1, previous_inspection_by_same_inspector := NA]  # first inspection has no previous


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


# Re-order columns.  ------------------------------------------------------
setcolorder(dat, 
            c("HSISID", "Date", 
              "Name", "Address1", "Address2", 
              "City", "State", "PostalCode", "PhoneNumber", 
              "RestaurantOpenDate", "days_from_open_date", 
              "FacilityType", "X", "Y", "GeocodeStatus", "zip",
              "Type", "Description", 
              "InspectedBy", "inspection_num", "inspector_ID",
              "previous_inspection_date", "days_since_previous_inspection", 
              "previous_inspection_by_same_inspector", 
              "Score", "num_critical", "num_non_critical", 
              "num_critical_previous", "num_non_critical_previous", 
              "num_critical_mean_previous", 
              "num_non_critical_mean_previous", 
              "avg_neighbor_num_critical", "avg_neighbor_num_non_critical", 
              "top_match", "second_match"))

# Write out the data. 
write_csv(dat, path = "raleigh/data/inspections.csv")
