
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
# [c(1:200000),]
waterDataTraining = trainingData
library(adabag)
library(rpart)
library(mlbench)

l <- length(waterDataTraining[,1])
sub <- sample(1:l,2*l/3)
mfinal <- 20
maxdepth <- 15

waterDataTraining$EVENT <- as.factor(waterDataTraining$EVENT)
water.adaboost <- boosting(EVENT~.,data=waterDataTraining[sub,], mfinal=mfinal,
                           control=rpart.control(maxdepth=maxdepth), coeflearn="Zhu")
# water.adaboost <- boosting(EVENT~.,data=waterDataTraining[sub,], mfinal=mfinal)
water.adaboost.pred <- predict(water.adaboost,newdata=waterDataTraining[-sub, ],type="class")
tb <- table(water.adaboost.pred$class, waterDataTraining$EVENT[-sub])
error.adaboost <- 1-(sum(diag(tb))/sum(tb))
tb
error.adaboost



#comparing error evolution in training and test set
errorevol(water.adaboost,newdata=waterDataTraining[sub, ])->evol.train
errorevol(water.adaboost,newdata=waterDataTraining[-sub, ])->evol.test

plot.errorevol(evol.test,evol.train)
png(filename="Error_depth15_mfinal20.png")
dev.off()


library(mlbench)
library(rpart)
data(Vehicle)
l <- length(Vehicle[,1])
sub <- sample(1:l,2*l/3)
mfinal <- 3 
maxdepth <- 5

Vehicle.rpart <- rpart(Class~.,data=Vehicle[sub,],maxdepth=maxdepth)
Vehicle.rpart.pred <- predict(Vehicle.rpart,newdata=Vehicle[-sub, ],type="class")
tb <- table(Vehicle.rpart.pred,Vehicle$Class[-sub])
error.rpart <- 1-(sum(diag(tb))/sum(tb))
tb
error.rpart

Vehicle.adaboost <- boosting(Class ~.,data=Vehicle[sub, ],mfinal=mfinal, coeflearn="Zhu",
                             control=rpart.control(maxdepth=maxdepth))
Vehicle.adaboost.pred <- predict.boosting(Vehicle.adaboost,newdata=Vehicle[-sub, ])
Vehicle.adaboost.pred$confusion
Vehicle.adaboost.pred$error

#comparing error evolution in training and test set
errorevol(Vehicle.adaboost,newdata=Vehicle[sub, ])->evol.train
errorevol(Vehicle.adaboost,newdata=Vehicle[-sub, ])->evol.test

plot.errorevol(evol.test,evol.train)

