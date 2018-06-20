library(adabag)
library(ROCR)
library(rpart)
library(xgboost)


l <- length(trainingData[,1])
adaboostError <- c()
xgError <- c()
results0.2 <- c()
results0.3 <- c()
results0.4 <- c()
results0.5 <- c()
results0.6 <- c()
results0.7 <- c()


for(i in 1:9){
  sub <- sample(1:l,i*l/10)
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

  water.adaboost <- boosting(EVENT~.,data=train, mfinal=mfinal,
                             control=rpart.control(maxdepth=maxdepth), coeflearn="Zhu")
  water.adaboost.pred <- predict(water.adaboost, newdata=test[, -11],type="class")
  tb <- table(water.adaboost.pred$class, test$EVENT)
  error.adaboost <- 1-(sum(diag(tb))/sum(tb))
  adaboostError <- c(adaboostError, error.adaboost)
  # tb
  # error.adaboost
  
  bst <- xgboost(data = data.matrix(train[,-11]), label = data.matrix(train[, 11]), nrounds =50, objective = "binary:logistic", nthread = 6, maxdepth = 5)
  water.xgboost.pred <- predict(bst, data.matrix(test[, -11]))
  err <- mean(as.numeric(water.xgboost.pred > 0.5) != test$EVENT)
  xgError <- c(xgError, err)
  
    # print(paste("test-error=", err))
  # 
  # tb <- table(as.numeric(water.xgboost.pred > 0.5), test$EVENT)
  # tb
  
  combined <- rowMeans(cbind(water.xgboost.pred, water.adaboost.pred$prob[,2]), na.rm=TRUE)
  combi_table0.2 <- table(as.numeric(combined > 0.2), test$EVENT)
  combi_table0.3 <- table(as.numeric(combined > 0.3), test$EVENT)
  combi_table0.4 <- table(as.numeric(combined > 0.4), test$EVENT)
  combi_table0.5 <- table(as.numeric(combined > 0.5), test$EVENT)
  combi_table0.6 <- table(as.numeric(combined > 0.6), test$EVENT)
  combi_table0.7 <- table(as.numeric(combined > 0.7), test$EVENT)
  
  results0.2 <- c(results0.2, 1-(sum(diag(combi_table0.2))/sum(combi_table0.2)))
  results0.3 <- c(results0.3, 1-(sum(diag(combi_table0.3))/sum(combi_table0.3)))  
  results0.4 <- c(results0.4, 1-(sum(diag(combi_table0.4))/sum(combi_table0.4)))
  results0.5 <- c(results0.5, 1-(sum(diag(combi_table0.5))/sum(combi_table0.5)))
  results0.6 <- c(results0.6, 1-(sum(diag(combi_table0.6))/sum(combi_table0.6)))
  results0.7 <- c(results0.7, 1-(sum(diag(combi_table0.7))/sum(combi_table0.7)))
  
  # print(table(as.numeric(combined > 0.26), test$EVENT))
  # print(0.0005373801 > error && 0.0004084089 > error)
  
}
print(c(mean(results0.2), 0.0005373801 > mean(results0.2) && 0.0004084089 > mean(results0.2)))

print(0.0005373801 > mean(results0.2) && 0.0004084089 > mean(results0.2))
print(mean(results0.2))
print(0.0005373801 > mean(results0.3) && 0.0004084089 > mean(results0.3))
print(mean(results0.3))
print(0.0005373801 > mean(results0.4) && 0.0004084089 > mean(results0.4))
print(mean(results0.4))
print(0.0005373801 > mean(results0.5) && 0.0004084089 > mean(results0.5))
print(mean(results0.5))
print(0.0005373801 > mean(results0.6) && 0.0004084089 > mean(results0.6))
print(mean(results0.6))
print(0.0005373801 > mean(results0.7) && 0.0004084089 > mean(results0.7))
print(mean(results0.7))

print(c(mean(xgError), mean(adaboostError)))

# xgError <- c(0.0013135897, 0.0006896366, 0.0003991934 ,0.0004896107 ,0.0004728945 ,0.0005015494, 0.0005254359, 0.0005015404, 0.0002865945)
# adaboostError <- c(0.0010031048, 0.0005552918, 0.0003684862, 0.0003582517, 0.0003439233, 0.0004119870, 0.0003582517, 0.0003224189, 0.0002149459)
# results0.3 <- c(0.0010269883, 0.0006090298, 0.0003582505, 0.0003701935, 0.0003152630, 0.0004836369, 0.0004299021, 0.0005373648, 0.0002865945)

# yrange = seq(10, 90, by=10)
# heading = "Error evolution vs train set size"
# plot(yrange, adaboostError, type='b', col="red", xlab = "Percentage of dataset used for training (%)", ylab="Test error (%)")
# lines(yrange, xgError, type='b', col="green")
# lines(yrange, results0.3, type='b', col="blue")
# legend(x="topright", y=0.92, legend=c("Adaboost", "XGBoost", "Ensemble of both"), col=c("red", "green", "blue"), lty=1, cex=0.8)
