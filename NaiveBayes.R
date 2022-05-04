library(tidyverse)
library(e1071)
library(e1071)
library(caTools)
library(caret)
path3<-"/Users/jamesmcsweeney/Desktop/WildFire CSC411 Project/R SCRIPTS/dataframe.csv"
dataframe<-read.csv(path3,header=TRUE)
dataframe$STAT_CAUSE=as.factor(dataframe$STAT_CAUSE)

# Splitting data into train
# and test data
split <- sample.split(dataframe, SplitRatio = 0.8)
train_cl <- subset(dataframe, split == "TRUE")
test_cl <- subset(dataframe, split == "FALSE")
train_control <- trainControl(method = "cv",
                              number = 10)
# Fitting Naive Bayes Model
# to training dataset
set.seed(120) # Setting Seed
classifier_cl <- naiveBayes(STAT_CAUSE ~ ., data = dataframe,trControl=train_control)
classifier_cl

# Predicting on test data'
y_pred <- predict(classifier_cl, newdata = test_cl)

# Confusion Matrix
cm <- table(test_cl$STAT_CAUSE, y_pred)
cm

# Model Evaluation
confusionMatrix(cm)
summary(dataframe)
NBx = dataframe[,-11]
NBy = dataframe$STAT_CAUSE
model = train(STAT_CAUSE~.,data=dataframe,method='nb',trControl=trainControl(method='cv',number=10))
model
warnings()
