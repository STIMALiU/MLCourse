# Logistic regression for spam data from the book Elements of Statistical Learning
# Author: Mattias Villani, Statistics and Machine Learning, Linkoping University, Sweden. e-mail: mattias.villani@liu.se

# Defining the log-likelihood function
LogLik <- function(betaVect,y,X){
  linFunc = X%*%betaVect # matrix product
  thetaVect = exp(linFunc)/(1+exp(linFunc))
  logLikelihood <- sum(y*log(thetaVect) + (1-y)*log(1-thetaVect))
}

# Reading in spam data from the ElemStatLearn package
#install.packages("ElemStatLearn")
library(ElemStatLearn)
data <- spam
SelectTraining <- sample(1:dim(data)[1], size = 3000, replace = FALSE)
y <- data[,58]
y <- ifelse(y=="spam",1,0)
X <- as.matrix(cbind(1,data[,1:57])) # Adding a column of ones for the intercept
nPara <- dim(X)[2]       # Number of covariates incl intercept
yTrain <- y[SelectTraining]
yTest <- y[-SelectTraining]
XTrain <- X[SelectTraining,]
XTest <- X[-SelectTraining,]

# Optimize to the find the ML estimates. control = list(fnscale=-1) puts a minus sign in front of LogLik
# We need this since optim minimizes, not maximizes.
initPar <- matrix(0,nPara,1)
optimResults <- optim(initPar, LogLik, gr = NULL, yTrain, XTrain, control=list(fnscale=-1))
betaHat <- optimResults$par

# Predition function
PredictLogistic <- function(threshold = 0.5, yTest, XTest, betaHat){
  linFunc = XTest%*%betaHat # matrix product
  thetaVect = exp(linFunc)/(1+exp(linFunc))
  results = list()
  results$probs <- thetaVect 
  results$preds <- ifelse(thetaVect>threshold,1,0)
  results$confusionMatrix <- table(results$preds,yTest)
  results$accuracy = sum(diag(results$confusionMatrix))/dim(XTest)[1] # What proportion of notes were correctly classified?
  results$precision = results$confusionMatrix[2,2]/sum(results$confusionMatrix[2,]) # Out of those selected (marked as fraud) what proportion were right? 
  results$recall = results$confusionMatrix[2,2]/sum(results$confusionMatrix[,2])# What proportion of frauds were detected? Sensitivity. True positive rate.
  results$FPR = results$confusionMatrix[2,1]/sum(results$confusionMatrix[,1]) # False Positive Rate
  return(results)
}

# Predicting the test set and evaluating the results with threshold = 0.5
results <- PredictLogistic(threshold = 0.5, yTest, XTest, betaHat)


##########   SVM  ###############
# install.packages('e1071')
# install.packages('rpart')
library(e1071)
library(rpart)

fitSVM <- svm(y = yTrain, x = XTrain, type ="C-classification")
predAllSVM <- predict(fitSVM, XTest, type = "class")
confusSVM <- table(as.matrix(predAllSVM),as.matrix(yTest))
accuracySVM <- sum(diag(confusSVM))/sum(confusSVM)
recallSVM <- confusSVM[2,2]/sum(confusSVM[,2])
precisionSVM <- confusSVM[2,2]/sum(confusSVM[2,])
c(accuracySVM,recallSVM,precisionSVM)
