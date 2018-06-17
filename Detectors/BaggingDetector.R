library(adabag)
library(rpart)

train <- function(dataset){
  #do something smart
  mfinal <- 10
  maxdepth <- 25
  minsplit <- 15
  water.bagging <- bagging(EVENT ~., data=dataset, mfinal=mfinal, 
                           control=rpart.control(maxdepth=maxdepth, minsplit=minsplit), par = TRUE)
  return(water.bagging)
}

detect <- function(dataset, booster){
  bagging.pred <- predict(booster,newdata=dataset,type="class")
  ## return prediction
  return(bagging.pred)
}

destruct <- function(){
  ## remove global variables

  ## delete files
}

getOutline <- function(){
  competitor.name <- "Robroek en Brenninkmeijer - BaggingDetector"
  competitor.institution <- "Radboud University"

  return (list(NAME=competitor.name, INSTITUTION=competitor.institution));
}