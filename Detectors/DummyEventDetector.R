train <- function(dataset)
  return(NULL)


detect <- function(dataset, booster){
  return(replicate(nrow(dataset), rand_val()))
}

rand_val <- function() {
  probability <- runif(1)
  event <- probability > 0.5
  return(event)
}

destruct <- function(){
  ## remove global variables
  
  ## delete files
}

getOutline <- function(){
  competitor.name <- "Max Muster"
  competitor.institution <- "Muster Uni"
  
  return (list(NAME=competitor.name, INSTITUTION=competitor.institution));
}