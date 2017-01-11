
# ##==============================================================================
# ## INITIALIZE
# ##==============================================================================
# ## Remove all objects; perform garbage collection
# rm(list=ls())
# gc(reset=TRUE)
# ## Detach libraries that are not used
# geneorama::detach_nonstandard_packages()
# ## Load libraries that are used
# geneorama::loadinstall_libraries(c("data.table", "MASS"))
# ## Load custom functions
# geneorama::sourceDir("CODE/functions/")
# 
# ## Import shift function
# shift <- geneorama::shift

library(data.table) 
library(readr)
library(magrittr)
library(lubridate)
library(sp)
library(geosphere)

options(stringsAsFactors=TRUE)

## ============================
## READ IN DATA FROM CSV FILES
## ============================
inspection.csv <- read_csv("../data/New_Food_Service_Inspections.csv")
inspection <- data.table(inspection.csv) 
rm(inspection.csv)

license.csv <- read_csv("../data/Active_Liquor_Licenses_in_Syracuse.csv")
license <- data.table(license.csv)
rm(license.csv)

## ================================================
## SUMMARIZE EACH INSPECTION FOR EACH RESTAURANT
## ================================================

dat <- NULL
restaurantId <- sort(unique(inspection[,`FACILITY CODE`]))
for (id in restaurantId) {
    inspection_thisRestaurant <- inspection[`FACILITY CODE` == id]
    inspection_thisRestaurant <- inspection_thisRestaurant[order(as.Date(inspection_thisRestaurant$`DATE OF INSPECTION`, format="%m/%d/%Y")),]
    
    facilityName = inspection_thisRestaurant[1,FACILITY]
    facilityZip = inspection_thisRestaurant[1,`FACILITY POSTAL ZIPCODE`]
    facilityType = inspection_thisRestaurant[1,`FOOD SERVICE DESCRIPTION`]
    lat = inspection_thisRestaurant[1,`LATITUDE`]
    lon = inspection_thisRestaurant[1,`LONGITUDE`]
    uniqueDates = unique(inspection_thisRestaurant[,`DATE OF INSPECTION`])
    
    licenseDist <- distGeo(c(lon,lat), as.matrix(license[ , list(Longitude,Latitude)]))
    nearestLicense <- min(licenseDist[!is.na(licenseDist)])
    sellsAlcohol = nearestLicense < 15 # assume if the distance to the nearest license is less than 15 meters, its in the list
    
    nPastCritical <- 0
    nPastNonCritical <- 0
    previousInspectionDate <- NULL
    for (date in uniqueDates) {
        inspection_thisDate <- inspection_thisRestaurant[`DATE OF INSPECTION`== date]
        nCritical <- sum(inspection_thisDate[,`CRITICAL VIOLATION`] == "Critical Violation")
        nNonCritical <- sum(inspection_thisDate[,`CRITICAL VIOLATION`] == "Not Critical Violation")
        inspectionType = unique(inspection_thisDate[,`INSPECTION TYPE`])
            
        daysRemainingOnPermit <- as.numeric(as.Date(unique(inspection_thisDate[,`PERMIT EXPIRATION DATE`]),format="%m/%d/%Y") - as.Date(date,format="%m/%d/%Y"))
        if (is.null(previousInspectionDate) == TRUE) {
            nPastCritical <- NA
            nPastNonCritical <- NA
            daysSinceLastInspection <- NA
        } else {
            daysSinceLastInspection <- as.numeric(as.Date(date,"%m/%d/%Y") - as.Date(previousInspectionDate,"%m/%d/%Y"))
        }
        
        testDate = "01/01/2016"
        if (as.Date(testDate,'%m/%d/%Y') - as.Date(date,"%m/%d/%Y") <= 0) {
            isTest <- TRUE
        } else {
            isTest <- FALSE
        }
        
        dat_thisRestaurant <- data.table(ID = id,
                                             name = facilityName,
                                             facilityType = facilityType,
                                             zip = facilityZip,
                                             date = date,
                                             inspectionType = inspectionType,
                                             nCritical = nCritical,
                                             nNonCritical = nNonCritical,
                                             nCritical_prev = nPastCritical,
                                             nNonCritical_prev = nPastNonCritical,
                                             daysTilExp = daysRemainingOnPermit,
                                             daysSincePrev = daysSinceLastInspection,
                                             alcLicense = sellsAlcohol,
                                             isTest = isTest,
                                             X = lon,
                                             Y = lat)
        
    if (is.null(dat)) {
        dat <- dat_thisRestaurant
    } else {
        dat <- rbind(dat,dat_thisRestaurant)
    }
        nPastCritical <- nCritical
        nPastNonCritical <- nNonCritical
        previousInspectionDate <- date
        
    }
}


# Compute nearest neighbors' critical violations. ------------------------------
dat <- subset(dat, !(X == 0 | Y == 0))  # ensure we have lat/long
dat[ , c("avg_neighbor_num_critical", "avg_neighbor_num_non_critical") := -1]
all_location <- as.matrix(dat[ , list(X, Y)])
dat_loc <- subset(dat, select = c("ID", "date", 
                                  "nCritical", "nNonCritical", 
                                  "X", "Y"))
n <- nrow(dat)
for (i in 1:n){
    print(paste("Proccesing", i, "of", n))
    curr_record <- dat[i,]
    curr_loc <- c(curr_record$X, curr_record$Y)
    curr_date <- curr_record$date
    curr_id <- curr_record$ID
    dat_loc$dist <- distGeo(curr_loc, as.matrix(dat_loc[ , list(X, Y)]))
    res <- dat_loc[date < curr_date & ID != curr_id][order(dist)][ 
        , .(mean(nCritical), mean(nNonCritical)), by = ID][1:5, ][ ,
          .(neigh_crit = mean(V1), neigh_non_crit = mean(V2), 
            top_match = ID[1], second_match = ID[2])]
    dat[ID == curr_id & date == curr_date, 
        `:=`(avg_neighbor_num_critical = res$neigh_crit, 
             avg_neighbor_num_non_critical = res$neigh_non_crit, 
             top_match = res$top_match, 
             second_match = res$second_match)]
}



saveRDS(dat,'../data/inspection2.Rds')
write.csv(dat,'../data/inspections2.csv')