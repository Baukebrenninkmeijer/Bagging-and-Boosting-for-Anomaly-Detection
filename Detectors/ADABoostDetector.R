library(adabag)
library(rpart)

train <- function(dataset){
  #do something smart
  # l <- length(dataset[,1])
  # sub <- sample(1:l,2*l/3)
  mfinal <- 20
  maxdepth <- 15
# 
#   waterdataset <- cbind(dataset)
# 
#   waterdataset$EVENT[waterdataset$EVENT == "True"] <- 1
#   waterdataset$EVENT[waterdataset$EVENT == "False"] <- 0
# 
#   waterdataset$EVENT <- as.factor(waterdataset$EVENT)
  # train <- waterdataset[sub,]
  water.adaboost <- boosting(EVENT~.,data=dataset, mfinal=mfinal,
                             control=rpart.control(maxdepth=maxdepth), coeflearn="Zhu")
  return(water.adaboost)
}

detect <- function(dataset, booster){
  adaboost.pred <- predict(booster,newdata=dataset,type="class")
  ## return prediction
  return(adaboost.pred)
}

destruct <- function(){
  ## remove global variables

  ## delete files
}

getOutline <- function(){
  competitor.name <- "Robroek en Brenninkmeijer - ADABoostDetector"
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
