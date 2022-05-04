#SVM
library(tidyverse)
library(e1071)
path3<-"/Users/jamesmcsweeney/Desktop/WildFire CSC411 Project/R SCRIPTS/dataframe.csv"
dataframe<-read.csv(path3,header=TRUE)
dataframe$STAT_CAUSE=as.factor(dataframe$STAT_CAUSE)
num_samples = dim(dataframe)[1]
split = sample.split(dataframe$STAT_CAUSE, SplitRatio = 0.8)

training_set = subset(dataframe, split == TRUE)
test_set = subset(dataframe, split == FALSE)
svmfit = svm(STAT_CAUSE ~ ., data = training_set, kernel = "linear", cost = 10, scale = FALSE)

ctrl <- trainControl(method="repeatedcv",   # 10fold cross validation
                     repeats=5,         # do 5 repetitions of cv
                     summaryFunction=twoClassSummary, 
                     classProbs=TRUE)
set.seed(1)
tune.out=tune(svm,STAT_CAUSE ~ ., data = training_set,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)),trControl=ctrl)
bestmod=tune.out$best.model
summary(bestmod)
print("best model uses cost .1")

classifier = svm(STAT_CAUSE ~ ., data = training_set, kernel = "linear", cost = .1, scale = FALSE)
summary(classifier)
ypred=predict(classifier,test_set)
table(predict=ypred, truth=test_set$STAT_CAUSE)



