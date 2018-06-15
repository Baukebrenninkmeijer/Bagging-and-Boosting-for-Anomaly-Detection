library(adabag)
library(rpart)

detect <- function(dataset){
  #do something smart
  l <- length(dataset[,1])
  sub <- sample(1:l,2*l/3)
  mfinal <- 20
  maxdepth <- 15

  train <- dataset[sub,]
  test <- dataset[-sub,]

  train$EVENT <- as.factor(train$EVENT)

  water.adaboost <- boosting(EVENT~.,data=train, mfinal=mfinal,
                             control=rpart.control(maxdepth=maxdepth), coeflearn="Zhu")
  water.adaboost.pred <- predict(water.adaboost,newdata=test,type="class")
  ## return prediction
  cat("hallo")
  return(water.adaboost.pred)
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



#
#
# tb <- table(water.adaboost.pred$class, waterDataTraining$EVENT[-sub])
# error.adaboost <- 1-(sum(diag(tb))/sum(tb))
# tb
# error.adaboost
#
#
#
# #comparing error evolution in training and test set
# errorevol(water.adaboost,newdata=waterDataTraining[sub, ])->evol.train
# errorevol(water.adaboost,newdata=waterDataTraining[-sub, ])->evol.test
# plot.errorevol(evol.test,evol.train)
