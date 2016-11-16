library(httr)
library(jsonlite)
library(dplyr)
library(plyr)
library(readr)
library(data.table)

api_key <- "AIzaSyB8ukIhuG0FaTfXXmoE7hJCmTftq21Yp5k"
googleUrl <- "https://maps.googleapis.com/maps/api/place/nearbysearch/json?"


params <- paste0("location=", row$Y, ",", row$X, "&",
                 "key=", api_key, "&",
                 "rankby=", "distance", "&",
                 "query=", paramName)
location_data <- GET(paste0(googleUrl, params))