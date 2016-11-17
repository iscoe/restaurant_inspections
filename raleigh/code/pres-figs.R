# Script to generate figures for presentation. 

suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(ROCR))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(bit64))
suppressPackageStartupMessages(library(randomForest))
suppressPackageStartupMessages(library(caret))


dat <- fread("../data/merged.csv", 
             colClasses = c("character", "integer", "character", "character", "numeric", 
                            "character", "character", "character", "integer", "integer", 
                            "character", "character", "character", "character", "character", 
                            "character", "character", "character", "numeric", "numeric", 
                            "character", "integer", "character", "integer", "integer", "numeric", 
                            "numeric", "character", "character", "integer", "integer", "integer", 
                            "numeric", "numeric", "numeric", "character", "character", "logical", 
                            "numeric", "integer", "character", "integer", "numeric", "numeric", 
                            "character", "logical", "logical", "logical", "logical", "logical", 
                            "logical", "logical", "logical", "logical", "logical", "logical", 
                            "logical", "logical", "logical", "logical", "logical", "logical", 
                            "logical", "logical", "logical"))
dat[ , zip := as.character(zip)]


## Data Description

# - Data from 9/21/2012 to 11/03/2016. 
# - 2,809 facilities (1,867 are restaurants)
# - Cities in Wake County (top 5: Raleigh, Cary, Wake Forest, Apex, Morrisville)


## Examine counts of critical vs. non-critical violations. 

p <- ggplot(dat, aes(num_critical)) + geom_histogram(binwidth = 1, color = "black", 
                                                fill = "lightblue") + 
  labs(title = "Histogram of Number of Critical Violations") + 
  xlab("Critical Violations") + 
  ylab("Count")
ggsave("raleigh/figs/pres-20161117/hist-crit.png", height = 6, width = 6)
ggplot(dat, aes(num_critical + num_non_critical, Score)) + geom_point() +
  labs(title = "Score vs. Number of All (Critical+Non-Critical) Violations") + 
  xlab("Critical + Non-Critical Violations") + 
  ylab("Score")
ggsave("raleigh/figs/pres-20161117/hist-crit.png", height = 6, width = 6)


## Violations by Facility Type 

ggplot(dat, aes(FacilityType, num_critical)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(title = "Critical Violations by Facility Type") + 
  xlab("Facility Type") + 
  ylab("Critical Violations")
ggsave("raleigh/figs/pres-20161117/hist-crit.png", height = 6, width = 6)



## Counts of inspections per restaurant

If we want to do time-series, we need to make sure we have enough observations
per restaurant. 

dat[ , .(num_inspections = uniqueN(Date)), by = HSISID] %>% 
  ggplot(aes(num_inspections)) + geom_histogram(binwidth = 1, color = "black", 
                                                fill = "lightblue") + 
  labs(title = "Number of Inspections per Restaurant") + 
  xlab("Number of Inspections") + 
  ylab("Count")


## Census data 

income_cols <- c("Median_family_income_dollars", 
                 "Median_household_income_dollars", 
                 "Per_capita_income_dollars", 
                 "Percent_Families_Below_Poverty_Line", 
                 "Percent_Food_Stamp/SNAP_benefits_in_the_past_12_months", 
                 "Percent_Supplemental_Security_Income")
dat_income <- dat[ , lapply(.SD, unique), by = zip, .SDcols = income_cols]
dat_income <- dat_income[ , !"zip", with = FALSE]
kable(cor(dat_income))

## Maps

![](../figs/leaflet.png)

## Maps

![](../figs/leaflet-2.png)

## Modelling

dat[ , num_critical_binary := factor(num_critical >= 1)]
yelp_cats <- c("hotdogs", "sandwiches", "pizza", "tradamerican", "burgers", 
               "mexican", "grocery", "breakfast_brunch", "coffee", "chinese", 
               "italian", "newamerican", "chicken_wings", "delis", "bars", 
               "salad", "seafood", "bbq", "bakeries", "sushi")
dat_model <- subset(dat, select = c("num_critical_binary", "Date",
                                    "num_critical_previous",
                                    "days_from_open_date",
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


fit <- glm(num_critical_binary ~ ., data = train, family = "binomial")
summary(fit)
fitted_values <- predict(fit, newdata = test, type = "response")

# Assess fit of logistic model. 

pred <- prediction(fitted_values, test$num_critical_binary)

# ROC.
plot(performance(pred, "tpr", "fpr"), main="ROC")
abline(0, 1, lty=2)

## sensitivity / specificity
plot(performance(pred, "sens", "spec"), main="sensitivity vs specificity")
abline(1, -1, lty=2)

## phi
plot(performance(pred, "phi"), main="phi scores")

## Fancy ROC curve:
op <- par(bg="lightgray", mai=c(1.2,1.5,1,1))
plot(performance(pred,"tpr","fpr"),
     main="ROC Curve", colorize=TRUE, lwd=10)
par(op)

## Effect of using a cost function on cutoffs
plot(performance(pred, "cost", cost.fp = 1, cost.fn = 1),
     main="Even costs (FP=1 TN=1)")
plot(performance(pred, "cost", cost.fp = 1, cost.fn = 4),
     main="Higher cost for FN (FP=1 TN=4)")

## Accuracy
plot(performance(pred, measure = "acc"))

# AUC.
performance(pred, measure = "auc")@y.values[[1]]  # AUC



ggplot(dat, aes(num_critical, Score)) + geom_point() +
  labs(title = "Score vs. Number of Critical Violations") + 
  xlab("Critical Violations") + 
  ylab("Score")

