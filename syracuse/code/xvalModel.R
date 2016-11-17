
library(data.table) 
library(readr)
library(magrittr)
library(lubridate)

dat <- readRDS("../data/inspection2.Rds")
dat <- dat[!is.na(`NUM CRITICAL VIOLATIONS (PREVIOUS INSPECTION`)]

##==============================================================================
## CREATE MODEL DATA
##==============================================================================
# sort(colnames(dat))

criticalFound <- (dat[,`NUM CRITICAL VIOLATIONS (THIS INSPECTION)`] > 0)
iiTrain <- dat[ , which(`USE FOR TESTING`== FALSE)]
iiTest <- dat[ , which(`USE FOR TESTING`== TRUE)]
dat <- dat[,`NUM CRITICAL VIOLATIONS (THIS INSPECTION)`:=NULL]
dat <- dat[,`USE FOR TESTING`:=NULL]
dat <- dat[,`FACILITY CODE`:=NULL]
dat <- dat[,`INSPECTION DATE`:=NULL]

mm <- model.matrix(criticalFound ~ . -1, data = dat)
mm <- as.data.table(mm)
str(mm)
colnames(mm)


##==============================================================================
## GLMNET MODEL
## FOR MORE INFO SEE:
## http://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
##==============================================================================

## Find best lambda based on CV results
## Note, The cvfit object includes the top model
cvfit <- cv.glmnet(x = as.matrix(dat[iiTrain]),
                   y = criticalFound[iiTrain],
                   family = "binomial", 
                   alpha = 0,
                   type.measure = "deviance")

## View of results
plot(cvfit)
cvfit$lambda
cvfit$lambda.min

##==============================================================================
## ATTACH PREDICTIONS TO DAT
##==============================================================================

## Attach predictions for top lambda choice to the data
dat$score <- predict(cvfit$glmnet.fit, 
                     newx = as.matrix(mm), 
                     s = cvfit$lambda.min,
                     type = "response")[,1]

## Identify each row as test / train
dat$Test <- 1:nrow(dat) %in% iiTest
dat$Train <- 1:nrow(dat) %in% iiTrain

## Calculate scores for all lambda values
allscores <- predict(cvfit$glmnet.fit, 
                     newx = as.matrix(mm), 
                     s = cvfit$glmnet.fit$lambda,
                     type = "response")

allscores <- as.data.table(allscores)
setnames(allscores, 
         cvfit$glmnet.fit$beta@Dimnames[[2]])

## Identify each row as test / train
allscores$Test <- 1:nrow(allscores) %in% iiTest
allscores$Train <- 1:nrow(allscores) %in% iiTrain