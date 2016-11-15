library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(data.table)

restaurant_names <- read_csv("raleigh/data/Restaurants_in_Wake_County.csv") %>% 
  data.table() %>%
  dplyr::select(Name,
                X,
                Y)

#appID <- "LKSRchDCQUVpr5Z_0V0s7A"
#appSecret <- "HHOjlap3IqRl9PDss7S1ZJclKJYjBUk8QEcirMH9SxNUVr622EZnrgg6tRDSll0f"
accessToken <- "sDLP7PHbl53ruD4taAbSUl3kezQED4blRTEuSRPvf0w5a7C9nrndLl1R8sl4_FzoFZDQoN_Jhl1YuU-EmIdg_lh9zQZrx-pVpEOXV9tKsWmzIrdVsu9jJ_KPeN0kWHYx"

yelpUrl <- "https://api.yelp.com/v3/businesses/search?"
term <- paste0("term=", "restaurants")
location <- paste0("location=", "21045")
limit <- paste0("limit=", "50")

yelpList <- list()
yelpCategories <- list()

features = c("name",
"is_closed",
"rating",
"review_count",
"location.address1",
"location.zip_code",
"coordinates.latitude",
"coordinates.longitude",
"price",
"phone")

for(i in 1:nrow(restaurant_names)){
  row <- restaurant_names[i,]
  #Tricky -doesn't handle spaces but we need to ensure we get the actual restaurant.  Using first word.
  #You can't merely delete the spaces because if there's a number after the name (indicating one of multiple
  #versions of this restaurant, ie Starbucks 54), it won't work.
  restaurantName <-sub(" [0-9]", "", sub("\\#.*", "", row$Name))
  paramName <- gsub("\\ ", "%20", restaurantName)
  params <- paste0("term=restaurants,", paramName, "&",
                    "latitude=", row$Y, "&", 
                   "longitude=", row$X, "&",
                   #"raidus=", "50", "&",
                   "sort_by=", "best_match", "&",
                   "limit=", "20")
  
  location_data <- GET(paste0(yelpUrl, params),
                       add_headers(Authorization = paste0("Bearer ",accessToken)))
  location_content <- content(location_data, type = "text");
  jsondat <- fromJSON(location_content);
  if("businesses" %in% names(jsondat) && length(jsondat$businesses) > 0){
      bus_df <- flatten(data.frame(jsondat$businesses));
      #sometimes not all of the columns get returned back (ie price).
      availableFeatures <- intersect(features, colnames(bus_df))
      df <- bus_df %>%
          dplyr::select(one_of(availableFeatures));
      
      # Extract categories as comma-separated string (will be split later).
      df$categories <- unlist(lapply(categories, 
                                     function(x) paste0(x$alias, collapse=",")))
      
      yelpList[[i]] <- df;
  }
}

yelpDF <- rbindlist(yelpList, use.names=TRUE, fill=TRUE, idcol=NULL)
write_csv(yelpDF[!duplicated(yelpDF),], "yelpData2.csv")