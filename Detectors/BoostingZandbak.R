
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

train <- function(dataset){
  
}

library(adabag)
library(pROC)
library(rpart)


l <- length(trainingData[,1])
sub <- sample(1:l,2*l/3)
mfinal <- 20
maxdepth <- 15

train <- trainingData[sub,]
test <- trainingData[-sub,]
train$EVENT[train$EVENT == "True"] <- 1
train$EVENT[train$EVENT == "False"] <- 0
test$EVENT[test$EVENT == "True"] <- 1
test$EVENT[test$EVENT == "False"] <- 0


train$EVENT <- as.factor(train$EVENT)
test$EVENT <- as.factor(test$EVENT)
mfinal <- 20
maxdepth <- 15
water.adaboost <- boosting(EVENT~.,data=train, mfinal=mfinal,
                           control=rpart.control(maxdepth=maxdepth), coeflearn="Zhu")
water.adaboost.pred <- predict(water.adaboost, newdata=test[, -11],type="class")
tb <- table(water.adaboost.pred$class, test$EVENT)
error.adaboost <- 1-(sum(diag(tb))/sum(tb))
tb
error.adaboost

boosting_roc <- roc(test$EVENT, as.numeric(water.adaboost.pred$prob[,2])^2, auc= TRUE, plot=TRUE, smooth=TRUE)
boosting_roc
plot(roc_results)
x <- boosting_roc
plot.roc(x, add=FALSE, reuse.auc=TRUE,
         axes=TRUE, legacy.axes=FALSE,
         # Generic arguments for par:
         xlim=if(x$percent){c(100, 0)} else{c(1, 0)},
         ylim=if(x$percent){c(0, 100)} else{c(0, 1)},
         xlab="Specificity",
         ylab=ifelse(x$percent, "Sensitivity (%)", "Sensitivity"),
         asp=1,
         mar=c(4, 4, 2, 2)+.1,
         mgp=c(2.5, 1, 0)
)

#comparing error evolution in training and test set
errorevol(water.adaboost, newdata=train)->evol.train
errorevol(water.adaboost, newdata=test)->evol.test
evol.train$error <- sqrt(evol.train$error)
evol.test$error <- sqrt(evol.test$error)
plot.errorevol(evol.test,evol.train)


library(xgboost)
# library(DiagrammeR)

bst <- xgboost(data = data.matrix(train[,-11]), label = data.matrix(train[, 11]), nrounds =50, objective = "binary:logistic", nthread = 6, maxdepth = 5)
water.xgboost.pred <- predict(bst, data.matrix(test[, -11]))
err <- mean(as.numeric(water.xgboost.pred > 0.5) != test$EVENT)
print(paste("test-error=", err))

tb <- table(as.numeric(water.xgboost.pred > 0.5), test$EVENT)
tb

combined <- as.numeric(rowMeans(cbind(water.xgboost.pred, water.adaboost.pred$prob[,2]), na.rm=TRUE)>0.5)


epochs <- 20

score_model <- function(model, 
                        epoch, 
                        data, 
                        datasetname) {
  pred <- predict(model, 
                  newdata = data[, -11], 
                  ntreelimit = epoch)
  # print(pred)
  
  if(class(pred) == "list"){
    pred = as.numeric(pred$class)
    data[,11] <- as.numeric(levels(data[,11]))[data[,11]]
  }
  # print(table(unlist(data[, 11])))
  # print(head(pred))
  # print(table(unlist(pred)))
  acc <- mean(data[,11] == ifelse(pred >=0.5, 1, 0))
  dev <- sigr::calcDeviance(pred, ifelse(data[,11] >=0.5, TRUE, FALSE))
  auc <- sigr::calcAUC(pred, ifelse(data[,11] >=0.5, TRUE, FALSE))
  data.frame(dataset = datasetname,
             epoch = epoch, 
             accuracy = acc,
             mean_deviance = dev/nrow(data),
             AUC = auc,
             stringsAsFactors = FALSE)
}

score_model_trajectory <- function(model, 
                                   epochs, 
                                   data, 
                                   datasetname) {
  # print("Komt wel hier?")
  evals <- lapply(epochs, function(epoch) {
                    score_model(model, 
                                epoch, 
                                data, 
                                datasetname)
                  })
  # print("nog hier")
  r <- dplyr::bind_rows(evals)
  colnames(r) <- paste(datasetname, 
                       colnames(r), 
                       sep = "_")
  r
}
specialTest <- test
specialTrain <- train
specialTest[, 11] <- as.numeric(as.factor(test[, 11])) -1
specialTrain[, 11] <- as.numeric(as.factor(train[,11])) -1

eval <- 
  cbind(
    score_model_trajectory(bst, 
                           seq_len(epochs), 
                           data.matrix(specialTrain), 
                           "train"),
    score_model_trajectory(bst, 
                           seq_len(epochs), 
                           data.matrix(specialTest), 
                           "test"))
cols <- c("train_epoch", "train_accuracy", 
          "train_mean_deviance", "train_AUC", 
          "test_accuracy", "test_mean_deviance", 
          "test_AUC")
eval <- eval[, cols, drop = FALSE]
knitr::kable(head(eval))

cT <- dplyr::tribble(
  ~measure,                 ~training,             ~validation,
  "minus mean deviance",    "train_mean_deviance", "train_mean_deviance",
  "accuracy",               "train_accuracy",      "test_accuracy",
  "AUC",                    "train_AUC",           "test_AUC"
)


library("xgboost")
library("WVPlots")
library("seplyr")
library("sigr")
library("kableExtra")
options(knitr.table.format = "html")

cT %.>%
  knitr::kable(.) %.>%
  kable_styling(., full_width = F) %.>%
  column_spec(., 2:3, background = "yellow")

plot_fit_trajectory(eval,
                   column_description = cT,
                   epoch_name = "train_epoch",
                   needs_flip = "minus mean deviance",
                   pick_metric = "minus mean deviance",
                   title = "xgboost performance trajectories")

library(rpart)
library(mlbench)

water.bagging <- bagging(EVENT ~., data=train, mfinal=10, control=rpart.control(maxdepth=25, minsplit=15), par = TRUE)
water.bagging.pred <- predict.bagging(water.bagging, newdata=test, newmfinal=3)
water.bagging.pred$confusion
water.bagging.pred$error

errorevol(water.bagging, newdata=train)->evol.train
errorevol(water.bagging, newdata=test)->evol.test
evol.train$error <- sqrt(evol.train$error)
evol.test$error <- sqrt(evol.test$error)
plot.errorevol(evol.test, evol.train)


pred <- prediction(c(water.bagging.pred$prob), c(test$EVENT))

library(pROC)
backup <- water.bagging.pred
train_roc <-train
test_roc <- test
water.bagging.pred <- backup
# train[,11] <- as.numeric(levels(train_roc[,11]))[train_roc[,11]]
test_roc[,11] <- as.numeric(levels(test_roc[,11]))[test_roc[,11]]
water.bagging.pred$class <- as.numeric(water.bagging.pred$class)
# water.bagging.pred$class <- as.numeric(levels(water.bagging.pred$pred))[water.bagging.pred$class]
  
print(head(test_roc$EVENT))
print(class(test_roc$EVENT))
print(head(water.bagging.pred$class))
print(class(water.bagging.pred$class))

roc_results <- roc(test_roc$EVENT, water.bagging.pred$class, auc= TRUE, plot=TRUE, smooth=TRUE)
plot(roc_results)
