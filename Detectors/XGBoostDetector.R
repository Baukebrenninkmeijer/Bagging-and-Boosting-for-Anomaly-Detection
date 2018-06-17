library(adabag)
library(rpart)

train <- function(dataset){
  #do something smart
  nrounds = 10
  nthread = 6
  maxdepth = 5
  water.xgb <- xgboost(data = data.matrix(dataset[,-11]), label = data.matrix(dataset[, 11]), 
                 nrounds = nrounds, objective = "binary:logistic", nthread = nthread, maxdepth = maxdepth)
  
  return(water.xgb)
}

detect <- function(dataset, booster){
  print("Komt wel hier")
  dataset <- testSet
  dataset$EVENT <- as.factor(dataset$EVENT)
  water.xgb.pred <- predict(booster, newdata=dataset, type="class")
  ## return prediction
  print("Komt niet meer hier")
  return(as.numeric(water.Xgb.pred > 0.5))
}

destruct <- function(){
  ## remove global variables

  ## delete files
}

getOutline <- function(){
  competitor.name <- "Robroek en Brenninkmeijer - XGBoostDetector"
  competitor.institution <- "Radboud University"

  return (list(NAME=competitor.name, INSTITUTION=competitor.institution));
}