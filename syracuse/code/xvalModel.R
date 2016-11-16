
library(data.table) 
library(readr)
library(magrittr)
library(lubridate)

dat <- readRDS("../data/inspection2.Rds")


##==============================================================================
## CREATE MODEL DATA
##==============================================================================
# sort(colnames(dat))

criticalFound <- (dat[,`NUM CRITICAL VIOLATIONS`] > 0)
dat <- dat[,`NUM CRITICAL VIOLATIONS`:=NULL]

mm <- model.matrix(criticalFound ~ . -1, data=xmat[ , -1, with=F])
mm <- as.data.table(mm)
str(mm)
colnames(mm)

##==============================================================================
## CREATE TEST / TRAIN PARTITIONS
##==============================================================================
iiTrain <- dat[ , which(`USE FOR TESTING`== FALSE)]
iiTest <- dat[ , which(`USE FOR TESTING`== TRUE)]

## Check to see if any rows didn't make it through the model.matrix formula
nrow(dat)
nrow(xmat)
nrow(mm)

##==============================================================================
## GLMNET MODEL
## FOR MORE INFO SEE:
## http://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
##==============================================================================

# fit ridge regression, alpha = 0, only inspector coefficients penalized
penalty <- ifelse(grepl("^Inspector", colnames(mm)), 1, 0)

## Find best lambda based on CV results
## Note, The cvfit object includes the top model
cvfit <- cv.glmnet(x = as.matrix(mm[iiTrain]),
                   y = xmat[iiTrain,  criticalFound],
                   family = "binomial", 
                   alpha = 0,
                   penalty.factor = penalty,
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