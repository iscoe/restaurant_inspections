
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

options(stringsAsFactors=TRUE)

## ============================
## READ IN DATA FROM CSV FILES
## ============================
inspection.csv <- read_csv("../data/New_Food_Service_Inspections.csv")
inspection <- data.table(inspection.csv) 
rm(inspection.csv)
## ================================================
## SUMMARIZE EACH INSPECTION FOR EACH RESTAURANT
## ================================================

inspection2 <- NULL
restaurantId <- sort(unique(inspection[,`FACILITY CODE`]))
for (id in restaurantId) {
    inspection_thisRestaurant <- inspection[`FACILITY CODE` == id]
    inspection_thisRestaurant <- inspection_thisRestaurant[order(as.Date(inspection_thisRestaurant$`DATE OF INSPECTION`, format="%m/%d/%Y")),]
    
    facilityName = inspection_thisRestaurant[1,FACILITY]
    facilityZip = inspection_thisRestaurant[1,`FACILITY POSTAL ZIPCODE`]
    facilityType = inspection_thisRestaurant[1,`FOOD SERVICE DESCRIPTION`]
    inspectionType = inspection_thisRestaurant[1,`INSPECTION TYPE`]
    uniqueDates = unique(inspection_thisRestaurant[,`DATE OF INSPECTION`])
    
    nPastCritical <- 0
    nPastNonCritical <- 0
    previousInspectionDate <- NULL
    for (date in uniqueDates) {
        inspection_thisDate <- inspection_thisRestaurant[`DATE OF INSPECTION`== date]
        nCritical <- sum(inspection_thisDate[,`CRITICAL VIOLATION`] == "Critical Violation")
        nNonCritical <- sum(inspection_thisDate[,`CRITICAL VIOLATION`] == "Not Critical Violation")
            
        daysRemainingOnPermit <- as.Date(unique(inspection_thisDate[,`PERMIT EXPIRATION DATE`]),format="%m/%d/%Y") - as.Date(date,format="%m/%d/%Y")
        if (is.null(previousInspectionDate) == TRUE) {
            previousInspectionDate <- date
            nPastCritical <- NA
            nPastNonCritical <- NA
        }
        
        daysSinceLastInspection <- as.Date(date,"%m/%d/%Y") - as.Date(previousInspectionDate,"%m/%d/%Y")
        
        # testDate = "01/01/2016"
        # if (as.Date(testDate,'%m/%d/%Y') - as.Date(date,"%m/%d/%Y") <= 0) {
        #     isTest <- TRUE
        # } else {
        #     isTest <- FALSE
        # }
        
        inspection2_thisRestaurant <- data.table('FACILITY CODE' = id,
                                             'FACILITY NAME' = facilityName,
                                             'FACILITY TYPE' = facilityType,
                                             'ZIP CODE' = facilityZip,
                                             'INSPECTION DATE' = date,
                                             'INSPECTION TYPE' = inspectionType,
                                             'NUM CRITICAL VIOLATIONS (THIS INSPECTION)' = nCritical,
                                             'NUM NON-CRITICAL VIOLATIONS (THIS INSPECTION)' = nNonCritical,
                                             'NUM CRITICAL VIOLATIONS (PREVIOUS INSPECTION)' = nPastCritical,
                                             'NUM NON-CRITICAL VIOLATIONS (PREVIOUS INSPECTION)' = nPastNonCritical,
                                             'DAYS UNTIL PERMIT EXPIRES' = daysRemainingOnPermit,
                                             'DAYS SINCE LAST INSPECTION' = daysSinceLastInspection)
        
    if (is.null(inspection2)) {
        inspection2 <- inspection2_thisRestaurant
    } else {
        inspection2 <- rbind(inspection2,inspection2_thisRestaurant)
    }
        nPastCritical <- nCritical
        nPastNonCritical <- nNonCritical
        
    }
}

saveRDS(inspection2,'../data/inspection2.Rds')
write.csv(inspection2,'../data/inspections2.csv')