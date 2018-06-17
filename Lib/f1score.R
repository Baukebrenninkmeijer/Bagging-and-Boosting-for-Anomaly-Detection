calculateScore <- function(observations, predictions){
  TruePositive <- sum(observations & predictions)
  FalsePositive <- sum(!observations & predictions)
  TrueNegative <- sum(!observations & !predictions)
  FalseNegative <- sum(observations & !predictions)
  
  PositivePredictiveValue <- TruePositive / sum(predictions) #precision 520/46522
  TruePositiveRate <- TruePositive / sum(observations) #recall 520/520
  
  # print(PositivePredictiveValue)
  # print(TruePositiveRate)
  # print(2 * PositivePredictiveValue * TruePositiveRate)
  # print(PositivePredictiveValue + TruePositiveRate)
  
  F1score <- (2 * PositivePredictiveValue * TruePositiveRate) / (PositivePredictiveValue + TruePositiveRate)
  
  if (is.nan(F1score)) F1score <- 0
  
  return(list(TP=TruePositive, FP=FalsePositive, TN=TrueNegative, FN=FalseNegative, SCORE= F1score))
}