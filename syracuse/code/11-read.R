# Read in and light munging on the Syracuse data. 

library(data.table) 
library(readr)
library(magrittr)
library(lubridate)
library(stringr)


#  Read in data. ----------------------------------------------------------
dat <- fread("syracuse/data/New_Food_Service_Inspections.csv")  # warnings fine


# Clean up column names.  ------------------------------------------------------
dat <- unique(dat)  # a few duplicate rows (~50 out of 35k)

# Standardize column names (make similar to other cities' column names). 
new_cols <- tolower(names(dat)) %>% 
  gsub("#", "_num_", .) %>% 
  gsub("[ ]+", "_", .) %>% 
  gsub("__", "_", .) %>% 
  gsub("\\.", "", .) 
setnames(dat, new_cols)

# Light clean up. 
dat[ , date_of_inspection := mdy(date_of_inspection)]  # convert to POSIXct
dat[ , facility := str_trim(facility)]  # at least one restaurant "Bi-Won" has trailing spaces


# Light exploratory. ------------------------------------------------------
# Quick look at completeness of data. 
dat[ , .N, by = floor_date(date_of_inspection, "month")] %>% 
  ggplot(aes(floor_date, N)) + 
  geom_point() + 
  geom_line() + 
  labs(title = "Number of Inspections per Month on Syracuse Data") + 
  xlab("Time")

# How many observations per restaurant. 
dat[ , .N, by = .(facility, address)][order(-N)]



# Aggregating to one inspection per row. ---------------------------------------

# If `violation_item` is "None" set any NA violations to zero, as they are almost 
# certainly zero. 
dat[violation_item == "None", mean(total_num_critical_violations, na.rm = TRUE)]
dat[violation_item == "None", mean(total_num_noncritical_violations, na.rm = TRUE)]
dat[violation_item == "None", mean(total_num_crit_not_corrected, na.rm = TRUE)]
dat[violation_item == "None" & is.na(total_num_critical_violations), 
    total_num_critical_violations := 0]
dat[violation_item == "None" & is.na(total_num_noncritical_violations), 
    total_num_noncritical_violations := 0]
dat[violation_item == "None" & is.na(total_num_crit_not_corrected), 
    total_num_crit_not_corrected := 0]

# Aggregate because a single inspection is spread across multiple rows. 
dat <- dat[ , .(num_critical = unique(total_num_critical_violations), 
         num_non_critical = unique(total_num_noncritical_violations), 
         num_critical_not_corrected = unique(total_num_crit_not_corrected)), 
         by = .(facility, address, zip = facility_postal_zipcode, 
                date_of_inspection, nys_health_operation_id, inspection_type, 
                food_service_type, food_service_description,
                lat = latitude, lng = longitude)]


# Compute previous value of violations. -----------------------------------
dat <- dat[order(facility, date_of_inspection)]
dat[ , num_critical_previous := shift(num_critical, 1, type = "lag"), 
     by = facility]
dat[ , num_non_critical_previous := shift(num_non_critical, 1, type = "lag"), 
     by = facility]
dat[ , num_non_critical_not_corrected_previous := 
       shift(num_critical_not_corrected, 1, type = "lag"), 
     by = facility]


# Write out data.  ----------------------------------------------------
write_csv(dat, path = "syracuse/data/inspections.csv")
