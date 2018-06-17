library(xgboost)


train <- function(dataset){
  #do something smart
  nrounds = 50
  nthread = 10
  maxdepth = 5
  water.xgb <- xgboost(data = data.matrix(dataset[,-11]), label = data.matrix(dataset[, 11]), 
                 nrounds = nrounds, objective = "binary:logistic", nthread = nthread, maxdepth = maxdepth)
  
  return(water.xgb)
}

detect <- function(dataset, booster){
  water.xgb.pred <- predict(booster, newdata=data.matrix(dataset), type="class")
  ## return prediction
  return(as.numeric(water.xgb.pred > 0.5))
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