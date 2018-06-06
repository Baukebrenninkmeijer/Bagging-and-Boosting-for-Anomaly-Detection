
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

require('adabag')
data(iris)
iris.adaboost <- boosting(Species~., data=iris, boos=TRUE, mfinal=3)
iris.adaboost
