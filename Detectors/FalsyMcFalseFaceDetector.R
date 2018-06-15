
detect <- function(dataset){
  ## Predict event as random guess with 50% probability
  probability <- runif(1)
  event <- probability > 1.0

  ## return prediction
  return(event)
}

destruct <- function(){
  ## remove global variables

  ## delete files
}

getOutline <- function(){
  competitor.name <- "Falsy McFalseFace"
  competitor.institution <- "Boatland"

  return (list(NAME=competitor.name, INSTITUTION=competitor.institution));
}
