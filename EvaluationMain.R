
###############################################################################
#                                                                             #
#      GECCO 2018 Industrial Challenge - Main Evaluation                      #
#                                                                             #
#      Run this file to test and evaluate your Detector                       #
#                                                                             #
###############################################################################

###############################################################################
### initialize workspace ######################################################
rm(list=ls());
set.seed(2);

baseDir <- getwd()
dataDir  <- paste(baseDir, "Data", sep="/")
submissionDir <- paste(baseDir, "Detectors", sep="/")
librariesDir  <- paste(baseDir, "Lib", sep="/")

setwd(librariesDir)
source("f1score.R")


###############################################################################
### read training data  #######################################################
#timeSeriesData <- data.frame(X1=(runif(n = 100)*100), X2=(runif(n = 100)*100), X3=(runif(n = 100)*100), EVENT=(runif(n = 100)+0.03)>=1, Prediction=NA)
setwd(dataDir)
trainingData <- readRDS(file = "waterDataTraining.RDS")

l <- length(trainingData[,1])
sub <- sample(1:l,2*l/3)

trainSet = trainingData[sub,]
testSet = trainingData[-sub,]

trainSet$EVENT[trainSet$EVENT == "True"] <- 1
trainSet$EVENT[trainSet$EVENT == "False"] <- 0
testSet$EVENT[testSet$EVENT == "True"] <- 1
testSet$EVENT[testSet$EVENT == "False"] <- 0
trainSet$EVENT <- as.factor(trainSet$EVENT)
testSet$EVENT <- as.factor(testSet$EVENT)

###############################################################################
### execute and evaluate all detectors ########################################
setwd(submissionDir)
allDetectors <- dir(pattern = "*XGBoostDetector.R")
cat(allDetectors)

completeResult <- NULL

for (submission in allDetectors){ # submission <- allDetectors[6]
  ## Load detector
  source(submission)
  submissionOutline <- getOutline()
  cat(paste("\nRunning Submission: ", submissionOutline$NAME))

  ## Run detector
  predictionResult <- rep(NA, nrow(testSet)) # empty result array
  booster <- train(trainSet)
  predictionResult <- detect(dataset = testSet[, -11], booster)

  ## Evaluate prediction using F1 score
  if(class(predictionResult) == "list"){
    predictionResult = as.numeric(predictionResult$class)
  }
  result <- calculateScore(observations = as.numeric(testSet$EVENT), predictions = predictionResult)

  ## Write evaluation result to result table
  SubmissionResult <- data.frame(SUBMISSION=submissionOutline$NAME, TP=result$TP, FP=result$FP, TN=result$TN, FN=result$FN, RESULT=result$SCORE, stringsAsFactors = FALSE)
  if (is.null(completeResult)){
    completeResult <- SubmissionResult
  } else {
    completeResult <- rbind(completeResult, SubmissionResult)
  }
}
cat("\nEvaluation finished:\n")
setwd(baseDir)

###############################################################################
### show results ##############################################################

## Largest value for result wins
winningIndex <- which(max(completeResult$RESULT) == completeResult$RESULT)
cat(paste("\nSubmission: *", completeResult$SUBMISSION[winningIndex], "* wins.\nSee data.frame: completeResult for more Details.", sep="" ))

completeResult
