library(adabag)
library(rpart)
library(xgboost)

train <- function(dataset){
  print("In train")
  mfinal <- 20
  maxdepth <- 15

  water.adaboost <- boosting(EVENT~.,data=dataset, mfinal=mfinal, control=rpart.control(maxdepth=maxdepth), coeflearn="Zhu")
  save(water.adaboost, file="adaboost.rda")
  
  bst <- xgboost(data = data.matrix(dataset[,-11]), label = data.matrix(dataset[, 11]), nrounds=50, objective = "binary:logistic", nthread = 6, maxdepth = 5)
  save(bst, file="bst.rda")
  return(c(water.adaboost, bst)))
}

detect <- function(dataset, booster){
  load("adaboost.rda")
  water.adaboost.pred <- predict(water.adaboost, newdata=dataset,type="class")
  load("bst.rda")
  water.xgb.pred <- predict(bst, newdata=data.matrix(dataset), type="class")
  combined <- rowMeans(cbind(water.xgb.pred, water.adaboost.pred$prob[,2]), na.rm=TRUE)
  return(as.numeric(combined > 0.3))
}

destruct <- function(){
  ## remove global variables
  
  ## delete files
}

getOutline <- function(){
  competitor.name <- "Robroek en Brenninkmeijer - EnsembleDetector"
  competitor.institution <- "Radboud University"
  
  return (list(NAME=competitor.name, INSTITUTION=competitor.institution));
}
