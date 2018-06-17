
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


l <- length(trainingData[,1])
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
test$EVENT <- as.factor(test$EVENT)

water.adaboost <- boosting(EVENT~.,data=train, mfinal=mfinal,
                           control=rpart.control(maxdepth=maxdepth), coeflearn="Zhu")
water.adaboost.pred <- predict(water.adaboost,newdata=test[, -11],type="class")
tb <- table(water.adaboost.pred$class, waterDataTraining$EVENT[-sub])
error.adaboost <- 1-(sum(diag(tb))/sum(tb))
tb
error.adaboost



#comparing error evolution in training and test set
errorevol(water.adaboost, newdata=train)->evol.train
errorevol(water.adaboost, newdata=test)->evol.test
plot.errorevol(evol.test,evol.train)


library(xgboost)
library(DiagrammeR)

dtrain <- xgb.DMatrix(data = data.matrix(train[,-11]), label = data.matrix(train[, 11]))
bst <- xgboost(data = data.matrix(train[,-11]), label = data.matrix(train[, 11]), nrounds =50, objective = "binary:logistic", nthread = 6, maxdepth = 5)
# , max.depth = 5, eta = 1, nthread = 2, nround = 2, objective = "binary:logistic"
# xgb_cv <- xgb.cv(data=dtrain, nrounds=50, objective = "binary:logistic", nfold = 5)
# xgb.plot.tree(feature_names = colnames(test[, -11]), model = bst)
watchlist <- list(train=dtrain, test=xgb.DMatrix(data = data.matrix(test[,-11]), label = data.matrix(test[, 11])))
bst <- xgb.train(data=dtrain, max.depth=5, nthread = 6, nround=40, watchlist=watchlist, eval.metric = "error", eval.metric = "logloss", objective = "binary:logistic")

water.xgboost.pred <- predict(bst, data.matrix(test[, -11]))
water.xgboostcv.pred <- predict(bst, data.matrix(test$EVENT))
err <- mean(as.numeric(water.xgboost.pred > 0.5) != test$EVENT)
print(paste("test-error=", err))

tb <- table(as.numeric(water.xgboost.pred > 0.5), test$EVENT)
tb


library(rpart)
library(mlbench)

water.bagging <- bagging(EVENT ~., data=train, mfinal=10, control=rpart.control(maxdepth=25, minsplit=15), par = TRUE)
water.bagging.pred <- predict.bagging(water.bagging, newdata=test, newmfinal=3)
water.bagging.pred$confusion
water.bagging.pred$error

errorevol(water.bagging, newdata=train)->evol.train
errorevol(water.bagging, newdata=test)->evol.test
plot.errorevol(evol.test, evol.train)
