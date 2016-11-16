yelpData1 <- read.csv("yelpData.csv")
yelpData2 <- read.csv("yelpData-2.csv")

yelpData <- rbind(yelpData1, yelpData2)
yelpData <- yelpData[, !(names(yelpData) == "X")]
yelpData <- unique(yelpData)

write.csv(yelpData, "raleigh/data/raleighYelpData.csv")

yelpCategories1 <- read.csv("yelpRestaurantCategories.csv")
yelpCategories2 <- read.csv("yelpRestaurantCategories-2.csv")
yelpCategories <- rbind(yelpCategories1, yelpCategories2)

yelpCategories <- yelpCategories[, !(names(yelpCategories) == "X")]
yelpCategories <- unique(yelpCategories)

all_false_cols <- apply(yelpCategories, MARGIN = 2, function(x){ all(x == FALSE)})
all_false_cols
yelpCategoriesNoNull <- yelpCategories[ , !all_false_cols]

write.csv(yelpCategories, "raleigh/data/raleighYelpAllCategories.csv")
write.csv(yelpCategoriesNoNull, "raleigh/data/raleighYelpNoNullCategories.csv")