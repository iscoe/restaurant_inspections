# Modelling for Raleigh data. 
# Code originally adapted from pres-nov17.Rnw. 



# Read data.  -------------------------------------------------------------
dat <- fread("raleigh/data/merged.csv", 
             colClasses = c("character", "integer", "character", "character", "character", 
                            "character", "character", "character", "character", "character", 
                            "character", "numeric", "character", "numeric", "numeric", "character", 
                            "character", "character", "character", "integer", "numeric", 
                            "character", "numeric", "logical", "numeric", "integer", "integer", 
                            "integer", "integer", "numeric", "numeric", "numeric", "numeric", 
                            "character", "character", "integer", "integer", "integer", "numeric", 
                            "numeric", "numeric", "character", "character", "logical", "numeric", 
                            "integer", "character", "integer", "numeric", "numeric", "character", 
                            "logical", "logical", "logical", "logical", "logical", "logical", 
                            "logical", "logical", "logical", "logical", "logical", "logical", 
                            "logical", "logical", "logical", "logical", "logical", "logical", 
                            "logical", "logical"))
dat[ , zip := as.character(zip)]
dat[grepl("Elderly Nutrition Sites", FacilityType), FacilityType := "Elderly Sites"]
dat[ , Date := ymd_hms(Date)]




# Set up model data.  -----------------------------------------------------
dat[ , num_critical_binary := factor(num_critical >= 1)]
yelp_cats <- c("hotdogs", "sandwiches", "pizza", "tradamerican", "burgers", 
               "mexican", "grocery", "breakfast_brunch", "coffee", "chinese", 
               "italian", "newamerican", "chicken_wings", "delis", "bars", 
               "salad", "seafood", "bbq", "bakeries", "sushi")
dat_model <- subset(dat, select = c("num_critical_binary", "Date",
                                    "FacilityType",
                                    "num_critical_previous",
                                    "num_non_critical_previous",
                                    "num_critical_mean_previous",
                                    "num_non_critical_mean_previous",
                                    "days_from_open_date",
                                    "days_since_previous_inspection",
                                    "Median_household_income_dollars",
                                    "Percent_Families_Below_Poverty_Line", 
                                    "avg_neighbor_num_critical", 
                                    "rating", "price", yelp_cats  # yelp
))
dat_model <- dat_model[complete.cases(dat_model)]  # make complete ONLY for quick modelling
dat_model[ , price := factor(price, levels = c("$", "$$", "$$$", "$$$$"))]

# Make train/test splits. 
train <- dat_model[Date < as.POSIXct("2016-01-01")]
test <- dat_model[Date >= as.POSIXct("2016-01-01")]
train[ , Date := NULL]
test[ , Date := NULL]



# Baseline logistic regression models.  -------------------------------------
# Fit a baseline model using only num_critical_previous as sole predictor. 
fit_baseline <- glm(num_critical_binary ~ num_critical_previous, data = train, family = "binomial")
fitted_baseline <- predict(fit_baseline, newdata = test, type = "response")
pred_baseline <- prediction(fitted_baseline, test$num_critical_binary)
round(performance(pred_baseline, measure = "auc")@y.values[[1]], 3)

fit <- glm(num_critical_binary ~ ., data = train, family = "binomial")
fitted_values <- predict(fit, newdata = test, type = "response")
pred <- prediction(fitted_values, test$num_critical_binary)
round(performance(pred, measure = "auc")@y.values[[1]], 3)

# ROC.
plot(performance(pred, "tpr", "fpr"), main="ROC", col = "blue")
plot(performance(pred_baseline, "tpr", "fpr"), main="ROC", add=TRUE, col = "red")
abline(0, 1, lty=2)



# Random forest classification model.  ----------------------------------------
rf <- randomForest(factor(num_critical_binary) ~ . - FacilityType, data = train)
varImpPlot(rf, main = "Variable Importance Plot (Random Forest)", cex = 0.75)


# Poisson model: TODO (Alex) ----------------------------------------------


