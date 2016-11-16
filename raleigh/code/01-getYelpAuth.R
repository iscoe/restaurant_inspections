library(httr)
library(jsonlite)

appID <- "LKSRchDCQUVpr5Z_0V0s7A"
appSecret <- "HHOjlap3IqRl9PDss7S1ZJclKJYjBUk8QEcirMH9SxNUVr622EZnrgg6tRDSll0f"

auth_url <- "https://api.yelp.com/oauth2/token"

auth_body = paste0("grant_type=", "client_credentials", "&", 
                   "client_id=", appID, "&", 
                   "client_secret=", appSecret)

auth_data <- POST(auth_url, body = auth_body, encode = "form")
auth_content <- content(auth_data, typ = "text")
jsondat <- fromJSON(auth_content)

accessToken = jsondat$access_token