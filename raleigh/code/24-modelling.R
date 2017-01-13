# Modelling for Raleigh data. 
# Code originally adapted from pres-nov17.Rnw. 

library(data.table) 
library(readr)
library(magrittr)
library(lubridate)
library(ggplot2)
library(plyr)
library(ROCR)
library(knitr)
library(hexbin)
library(RColorBrewer)
library(sp)
library(geosphere)
library(randomForest)
library(glmnet)
library(xtable)
source("syracuse/code/simulated_date_diff_mean.R")

inspectors_differ <- FALSE  # CHANGE here whether to subset to inspectors that differ (FALSE = all of the data)

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
# Function to set up the model data. Used twice: once on original data, and again
# examining model performance when inspectors differ. 
setupModelData <- function(dat, subset_columns, inspectors_differ = FALSE){
  # Subset the data. 
  if (inspectors_differ == TRUE){
    dat <- dat[previous_inspection_by_same_inspector == FALSE]
  }
  dat_model <- subset(dat, select = subset_columns)
  dat_model <- dat_model[complete.cases(dat_model)]  # make complete ONLY for quick modelling
  dat_model[ , .(N = .N, unique_HSISID = uniqueN(HSISID))] %>% print()
  dat_model[ , HSISID := NULL]
  
  # Compute estimate of how often previous inspection by same inspector.
  # Note: the data are not exactly the same but only differ by < 5 rows.
  dat_model2 <- subset(dat, select = c(subset_columns, "previous_inspection_by_same_inspector"))
  dat_model2 <- dat_model2[complete.cases(dat_model2)]
  perc <- dat_model2[ , mean(previous_inspection_by_same_inspector, na.rm = TRUE)] 
  print(paste("The percentage of previous inspections made by same inspector:", perc))
  
  dat_model[ , .N, by = FacilityType][order(-N)] %>% kable()
  dat_model[ , .N, by = FacilityType][order(-N)] %>% xtable()
  
  # Make factors into numeric.
  if ("num_critical_binary" %in% names(dat_model)){
    dat_model[ , num_critical_binary := as.numeric(num_critical_binary == TRUE)]
  }
  # Convert price into numeric. 
  dat_model[ , price := factor(price, levels = c("$", "$$", "$$$", "$$$$"))]  # make factor
  price <- model.matrix(~ price - 1, data = dat_model) %>% as.data.table()
  setnames(price, c("price1", "price2", "price3", "price4"))
  dat_model <- cbind(dat_model, price)
  dat_model[ , price := NULL]  # no longer needed as it is has cbind'ed in
  # Convert FacilityType into numeric. 
  facility_type <- model.matrix(~ FacilityType - 1, data = dat_model) %>% as.data.table()
  setnames(facility_type, gsub(" ", "_", names(facility_type)))
  dat_model <- cbind(dat_model, facility_type)
  dat_model[ , FacilityType := NULL]  # no longer needed as it is has cbind'ed in
  
  dat_model[ , train := Date < as.POSIXct("2016-01-01")]
  dat_model[ , .N, by = train] %>% print()
  return(dat_model)
}

yelp_cats <- c("hotdogs", "sandwiches", "pizza", "tradamerican", "burgers", 
               "mexican", "grocery", "breakfast_brunch", "coffee", "chinese", 
               "italian", "newamerican", "chicken_wings", "delis", "bars", 
               "salad", "seafood", "bbq", "bakeries", "sushi")
dat_model <- setupModelData(dat, c("num_critical_binary", "Date", "HSISID",  # HSISID only used for counts, then removed
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
                                   "rating", "price", yelp_cats), 
                            inspectors_differ)

# Make train/test splits.  ------------------------------------------------
X_train <- dat_model[train == TRUE, !c("train", "num_critical_binary", "Date"), with = FALSE] %>% 
  as.matrix()
Y_train <- dat_model[train == TRUE, "num_critical_binary", with = FALSE] %>% as.matrix()
X_test <- dat_model[train == FALSE, !c("train", "num_critical_binary", "Date"), with = FALSE] %>% 
  as.matrix()
Y_test <- dat_model[train == FALSE, "num_critical_binary", with = FALSE] %>% as.matrix()


# Baseline logistic regression models.  -------------------------------------
# Initialize AUC data.frame. 
auc_df <- data.frame(model = rep(NA, 6), AUC = rep(NA, 6), days_saved = rep(NA, 6))
png(paste0("raleigh/figs/roc", ifelse(inspectors_differ, "-inspectors-differ.png", ".png")),
    width = 600, height = 400)

# Fit a baseline model using only num_critical_previous as sole predictor. 
fit_baseline <- glm(num_critical_binary ~ num_critical_previous, 
                    data = dat_model[train == TRUE, !"train", with = FALSE], 
                    family = "binomial")
fitted_baseline <- predict(fit_baseline, 
                           newdata = dat_model[train == FALSE, !"train", with = FALSE],
                           type = "response")
pred <- prediction(fitted_baseline, dat_model[train == FALSE, num_critical_binary])
plot(performance(pred, "tpr", "fpr"), main="ROC", col = "black")
abline(0, 1, lty=2)

# Run the Chicago simulation.
dat_model[ , Date := as.Date(Date)]
index_first_two_months <- dat_model[train == FALSE, which(Date <= as.POSIXct("2016-03-04"))]
auc_df[1,] <- c("Baseline", 
                round(performance(pred, measure = "auc")@y.values[[1]], 3),
                round(
                  simulated_date_diff_mean(dates = dat_model[train==FALSE][index_first_two_months, Date], 
                                           scores = fitted_baseline[index_first_two_months], 
                                           pos = dat_model[train == FALSE][index_first_two_months, num_critical_binary]), 
                  3))

fit <- glm(num_critical_binary ~ ., 
           data = dat_model[train == TRUE, !"train", with = FALSE], 
           family = "binomial")
fitted_values <- predict(fit, 
                         newdata = dat_model[train == FALSE, !"train", with = FALSE],
                         type = "response")
pred <- prediction(fitted_values, dat_model[train == FALSE, num_critical_binary])
auc_df[2,] <- c("Logistic", 
                round(performance(pred, measure = "auc")@y.values[[1]], 3), 
                round(simulated_date_diff_mean(dates = dat_model[train==FALSE][index_first_two_months, Date], 
                                               scores = fitted_values[index_first_two_months], 
                                               pos = dat_model[train == FALSE][index_first_two_months, num_critical_binary]), 3))
plot(performance(pred, "tpr", "fpr"), main="ROC", add = TRUE, col = "blue")


# Now that the simulation is complete, remove Date. 
# dat_model[ , Date := NULL]  # date no longer needed

# Lasso classification with glmnet. -------------------------------------------
# Logistic regression with L1 regularization. 
fit_lasso <- glmnet(X_train, Y_train, family = "binomial", alpha = 1)
Yhat_test <- predict(fit_lasso, X_test, type = "link")
pred <- prediction(Yhat_test, matrix(Y_test,nrow=nrow(Yhat_test),
                                     ncol=ncol(Yhat_test)))
perf <- performance(pred,"auc")@y.values
bestInd <- which.max(unlist(perf))
pred <- prediction(Yhat_test[,bestInd], Y_test)
plot(performance(pred, "tpr", "fpr"), add = TRUE, col = "darkgreen")
auc_df[3,] <- c("Logistic L1 Reg", 
                round(performance(pred, measure = "auc")@y.values[[1]], 3), 
                round(simulated_date_diff_mean(dates = dat_model[train==FALSE][index_first_two_months, Date], 
                                               scores = pred@predictions[[1]][index_first_two_months], 
                                               pos = Y_test[index_first_two_months]), 3))

# Logistic with L2 regularization only. 
fit <- glmnet(X_train,Y_train,alpha=0)
Yhat_test <- predict(fit,X_test,type="link")
pred <- prediction(Yhat_test, matrix(Y_test,nrow=nrow(Yhat_test),
                                     ncol=ncol(Yhat_test)))
perf = performance(pred,"auc")@y.values
bestInd = which.max(unlist(perf))
pred <- prediction(Yhat_test[,bestInd], Y_test)
plot(performance(pred, "tpr", "fpr"), add = TRUE, col = "red")
fit <- glmnet(X_train,Y_train,alpha=0,lambda=fit$lambda[bestInd])
coef = coefficients(fit)
auc_df[4,] <- c("Logistic L2 Reg", 
                round(performance(pred, measure = "auc")@y.values[[1]], 3), 
                round(simulated_date_diff_mean(dates = dat_model[train==FALSE][index_first_two_months, Date], 
                                               scores = pred@predictions[[1]][index_first_two_months], 
                                               pos = Y_test[index_first_two_months]), 3))


# Logistic with both l1 and l2
fit <- glmnet(X_train,Y_train,alpha=0.5)
Yhat_test <- predict(fit,X_test,type="link")
pred <- prediction(Yhat_test, matrix(Y_test,nrow=nrow(Yhat_test),
                                     ncol=ncol(Yhat_test)))
perf = performance(pred,"auc")@y.values
bestInd = which.max(unlist(perf))
pred <- prediction(Yhat_test[,bestInd], Y_test)
plot(performance(pred, "tpr", "fpr"), col = "violet", add = TRUE)
fit <- glmnet(X_train,Y_train,alpha=0.5,lambda=fit$lambda[bestInd])
coef = coefficients(fit)
auc_df[5,] <- c("Logistic L1+L2 Reg", 
                round(performance(pred, measure = "auc")@y.values[[1]], 3), 
                round(simulated_date_diff_mean(dates = dat_model[train==FALSE][index_first_two_months, Date], 
                                               scores = pred@predictions[[1]][index_first_two_months], 
                                               pos = Y_test[index_first_two_months]), 3))


# Random forest classification model.  ----------------------------------------
fit_RF <- randomForest(X_train, as.factor(Y_train), importance = TRUE)
Yhat_test <- predict(fit_RF, X_test, type = "prob")
Yhat_test <- Yhat_test[,-1]
pred <- prediction(Yhat_test, Y_test)
perf = performance(pred,"auc")@y.values
plot(performance(pred, "tpr", "fpr"), add=TRUE, col = "orange")
legend("bottomright", c('Baseline', 'Logistic', 'Logistic, L1 Reg', 
                        'Logistic, L2 Reg','Logistic, L1+L2 Reg', 'Random Forest', 
                        'Chance Level'),
       col=c('black','blue','darkgreen','red','violet','orange', 'black'),
       lty=c(1,1,1,1,1,1,2), cex = 0.95)
dev.off()  # close figure
auc_df[6,] <- c("Random Forest", 
                round(performance(pred, measure = "auc")@y.values[[1]], 3), 
                round(simulated_date_diff_mean(dates = dat_model[train==FALSE][index_first_two_months, Date], 
                                               scores = pred@predictions[[1]][index_first_two_months], 
                                               pos = Y_test[index_first_two_months]), 3))

# Random forest variable importance. 
png(paste0("raleigh/figs/var-imp", ifelse(inspectors_differ, "-inspectors-differ.png", ".png")),
    width = 600, height = 400)
varImpPlot(fit_RF, main = "Random Forest Variable Importance", cex = 0.75)
dev.off()

kable(auc_df)
xtable(auc_df)


# Baseline model with Inspector Information.  -----------------------------
dat_model_inspector_full <- setupModelData(dat, c("num_critical_binary",
                                                  "num_critical",
                                                  "Date", "HSISID",  # HSISID only used for counts, then removed
                                                  "FacilityType", "InspectedBy",
                                                  "num_critical_previous",
                                                  "num_non_critical_previous",
                                                  "num_critical_mean_previous",
                                                  "num_non_critical_mean_previous",
                                                  "days_from_open_date",
                                                  "days_since_previous_inspection",
                                                  "Median_household_income_dollars",
                                                  "Percent_Families_Below_Poverty_Line", 
                                                  "avg_neighbor_num_critical", 
                                                  "rating", "price", yelp_cats), 
                                           inspectors_differ = FALSE)
# Need to exclude certain inspectors who are in training set but not in test set. 
dat_model_inspector_full[ , Date := NULL]  # date no longer needed
dat_model_inspector_full[ , InspectedBy := as.character(InspectedBy)]
dat_model_inspector_full <- subset(dat_model_inspector_full, 
                                   !(InspectedBy %in% c("Ginger Johnson", 
                                                        "Jessica Andrews", 
                                                        "Marion Wearing")))
dat_model_inspector_full[ , .N, by = train] %>% print()

# Make dataset just for classification. 
dat_model_inspector_1 <- subset(dat_model_inspector_full, 
                                select = c("num_critical_binary", "InspectedBy", "train"))
dat_model_inspector_1[ , InspectedBy := as.character(InspectedBy)]

fit_baseline <- glm(num_critical_binary ~ InspectedBy, 
                    data = dat_model_inspector_1[train == TRUE, !"train", with = FALSE], 
                    family = "binomial")
fitted_baseline <- predict(fit_baseline, 
                           newdata = dat_model_inspector_1[train == FALSE, !"train", with = FALSE],
                           type = "response")
pred <- prediction(fitted_baseline, dat_model_inspector_1[train == FALSE, num_critical_binary])
plot(performance(pred, "tpr", "fpr"), main="ROC", col = "black")
abline(0, 1, lty=2)
print(paste("AUC for inspector model:",
            round(performance(pred, measure = "auc")@y.values[[1]], 3)))

# Poisson model. 
dat_model_inspector_2 <- subset(dat_model_inspector_full, 
                                select = c("num_critical", "InspectedBy", "train"))
dat_model_inspector_2[ , .N, by = train] %>% print()
fit_baseline <- glm(num_critical ~ InspectedBy, 
                    data = dat_model_inspector_2[train == TRUE, !"train", with = FALSE], 
                    family = "poisson")
fitted_baseline <- predict(fit_baseline, 
                           newdata = dat_model_inspector_2[train == FALSE, !"train", with = FALSE],
                           type = "response")
Y_test <- dat_model_inspector_2$num_critical[dat_model_inspector_2$train == FALSE]
plot(fitted_baseline, Y_test, pch = 19)
RMSE <- sqrt(mean((Y_test - fitted_baseline)^2))
print(paste("The RMSE for Poisson model is:", RMSE))



# Poisson model ----------------------------------------------
dat_model <- setupModelData(dat, c("num_critical", "Date", "HSISID",  # HSISID only used for counts, then removed
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
                                   "rating", "price", yelp_cats), 
                            inspectors_differ)
dat_model[ , Date := as.Date(Date)]  # date no longer needed
# Make train/test splits.  
X_train <- dat_model[train == TRUE, !c("train", "num_critical", "Date"), with = FALSE] %>% 
  as.matrix()
Y_train <- dat_model[train == TRUE, "num_critical", with = FALSE] %>% as.matrix()
X_test <- dat_model[train == FALSE, !c("train", "num_critical", "Date"), with = FALSE] %>% 
  as.matrix()
Y_test <- dat_model[train == FALSE, "num_critical", with = FALSE] %>% as.matrix()

# Poisson model with L1 regularization. 
cvfit <- cv.glmnet(X_train, Y_train, family = "poisson", alpha = 1)
plot(cvfit)
cvfit$lambda.min
cvfit$lambda.1se
coef(cvfit, s = "lambda.min") %>% as.matrix() %>% round(digits = 2)
coef(cvfit, s = "lambda.1se") %>% as.matrix() %>% round(digits = 2)
Yhat_test <- predict(cvfit, newx = X_test, s = "lambda.min", type = "response")
RMSE <- sqrt(mean((Y_test - Yhat_test)^2))
print(paste0("The RMSE for Poisson model is (inspectors differ = ", inspectors_differ, 
             ") is ", RMSE))
png(paste0("raleigh/figs/poisson-predicted" , 
           ifelse(inspectors_differ == TRUE, "-inspectors-differ.png", ".png")),
    width = 600, height = 350)
plot(Yhat_test, Y_test, pch = 19, cex = 0.5, 
     main = "Actual vs. Predicted Number of Critical Violations\nfor Poisson Model with L1 Regularization", 
     xlab = "Predicted Critical Violations", 
     ylab = "Actual Critical Violations")
dev.off()

# Look at most important variables.
coef(cvfit, s = "lambda.min") %>% as.matrix() %>% 
  round(digits = 3) %>% as.data.frame()
coef_poisson <- coef(cvfit, s = "lambda.1se") %>% as.matrix() %>% 
  round(digits = 3) %>% as.data.frame()
coef_poisson$Variable <- rownames(coef_poisson)
coef_poisson <- as.data.table(coef_poisson)
setnames(coef_poisson, c("Coefficient", "Variable"))
setcolorder(coef_poisson, c("Variable", "Coefficient"))
coef_poisson <- coef_poisson[Coefficient > 0][order(-Coefficient)]
kable(coef_poisson)
xtable(coef_poisson)

# Run Chicago simulation. 
days_saved_poisson <- simulated_date_diff_mean(dates = dat_model[train==FALSE][index_first_two_months, Date], 
                         scores = -Yhat_test[index_first_two_months], 
                         pos = Y_test[index_first_two_months])
print(paste("The days saved for Poisson model is:", 
            round(as.numeric(days_saved_poisson), 2)))
