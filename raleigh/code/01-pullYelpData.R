library(httr)
library(jsonlite)
library(dplyr)
library(plyr)
library(readr)
library(data.table)

writeData <- function(yelpList, categoryList){
  yelpDF <- rbindlist(yelpList, use.names=TRUE, fill=TRUE, idcol=NULL)
  yelpDF <- unique(yelpDF)  # delete duplicates
  write.table(yelpDF, file = "yelpData.csv", append = TRUE, row.names = TRUE, sep = ",")
  categoryDF <- rbindList(categoryList, use.names=TRUE, fill=TRUE< idcol=NULL)
  write.table(categoryDF, file = "yelpRestaurantCategories.csv", append = TRUE, row.names = TRUE, sep = ",")
}

accessToken <- "sDLP7PHbl53ruD4taAbSUl3kezQED4blRTEuSRPvf0w5a7C9nrndLl1R8sl4_FzoFZDQoN_Jhl1YuU-EmIdg_lh9zQZrx-pVpEOXV9tKsWmzIrdVsu9jJ_KPeN0kWHYx"
yelpUrl <- "https://api.yelp.com/v3/businesses/search?"
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


restaurant_names <- read_csv("raleigh/data/Restaurants_in_Wake_County.csv") %>% 
  data.table() %>%
  dplyr::select(Name,
                X,
                Y)

yelpCategories <- read_csv("yelpWork/foodCategories.csv")$CATEGORIES
numCats <- length(yelpCategories)

yelpList <- list();
categoryList <- list();

num_restaurants <- nrow(restaurant_names)

for(i in 1:num_restaurants){
  print(paste("Processing", i, "of", num_restaurants))
  row <- restaurant_names[i,]
  #Tricky -doesn't handle spaces but we need to ensure we get the actual restaurant.  Using first word.
  #You can't merely delete the spaces because if there's a number after the name (indicating one of multiple
  #versions of this restaurant, ie Starbucks 54), it won't work.
  restaurantName <-sub(" [0-9]", "", sub("\\#.*", "", row$Name))
  paramName <- gsub("\\ ", "%20", restaurantName)
  params <- paste0("term=restaurants,", paramName, "&",
                    "latitude=", row$Y, "&",
                   "longitude=", row$X, "&",
                   "sort_by=", "best_match", "&",
                   "limit=", "20")
  location_data <- GET(paste0(yelpUrl, params),
                       add_headers(Authorization = paste0("Bearer ",accessToken)))
  location_content <- content(location_data, type = "text", encoding = "UTF-8");
  jsondat <- fromJSON(location_content);
  if("businesses" %in% names(jsondat) && length(jsondat$businesses) > 0){
      bus_df <- flatten(data.frame(jsondat$businesses));
      #sometimes not all of the columns get returned back (ie price).
      availableFeatures <- intersect(features, colnames(bus_df))
      df <- bus_df %>%
          dplyr::select(one_of(availableFeatures));
      
      # Extract categories as comma-separated string (will be split later).
      df$categories <- unlist(lapply(bus_df$categories, 
                                     function(x) paste0(x$alias, collapse=",")))
      bus_cats <- bus_df[,c("id","categories")];
      bus_categoryDF <- setNames(data.frame(matrix(FALSE, ncol = numCats, nrow = length(bus_cats$id))), yelpCategories)
      row.names(df) <- bus_df$id
      row.names(bus_categoryDF) <- bus_df$id
      for(i in 1:nrow(bus_cats)){
        id <- bus_cats[i,]$id
        cats <- intersect(bus_cats[i,]$categories[[1]]$alias, yelpCategories)
        bus_categoryDF[id,cats] <- TRUE
      }
      yelpCategories[[i]] <- bus_categoryDF;
      yelpList[[i]] <- df;
 
  if(i %% 50 == 0){
    writeData(yelpList, categoryList)
    yelpList <- list();
    categoryList <- list();
  }
}

writeData(yelpList, categoryList)
