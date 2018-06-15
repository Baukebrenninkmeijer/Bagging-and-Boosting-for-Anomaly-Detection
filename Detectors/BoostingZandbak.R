
detect <- function(dataset){
  ## Predict event as random guess with 50% probability
  probability <- runif(1)
  event <- probability > 0.5
  
  ## return prediction
  return(event)
}

destruct <- function(){
  ## remove global variables
  
  ## delete files
}

getOutline <- function(){
  competitor.name <- "Robroek en Brenninkmeijer"
  competitor.institution <- "Radboud University"
  
  return (list(NAME=competitor.name, INSTITUTION=competitor.institution));
}

train <- function(dataset){
  
}

library(adabag)
library(ROCR)
library(rpart)


l <- length(waterDataTraining[,1])
sub <- sample(1:l,2*l/3)
mfinal <- 20
maxdepth <- 15

train <- trainingData[sub,]
test <- trainingData[-sub,]
train$EVENT[train$EVENT == "True"] <- 1
train$EVENT[train$EVENT == "False"] <- 0
test$EVENT[test$EVENT == "True"] <- 1
test$EVENT[test$EVENT == "False"] <- 0


train$EVENT <- as.factor(train$EVENT)

water.adaboost <- boosting(EVENT~.,data=train, mfinal=mfinal,
                           control=rpart.control(maxdepth=maxdepth), coeflearn="Zhu")
water.adaboost.pred <- predict(water.adaboost,newdata=test,type="class")
tb <- table(water.adaboost.pred$class, waterDataTraining$EVENT[-sub])
error.adaboost <- 1-(sum(diag(tb))/sum(tb))
tb
error.adaboost



#comparing error evolution in training and test set
errorevol(water.adaboost,newdata=waterDataTraining[sub, ])->evol.train
errorevol(water.adaboost,newdata=waterDataTraining[-sub, ])->evol.test
plot.errorevol(evol.test,evol.train)


library(xgboost)

dtrain <- xgb.DMatrix(data = data.matrix(train[,-11]), label = data.matrix(train[, 11]))
bst <- xgboost(data = dtrain, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "binary:logistic")
pred <- predict(bst, data.matrix(test_gx$EVENT))
print(length(pred))
err <- mean(as.numeric(pred > 0.5) != test$EVENT)
print(paste("test-error=", err))
