library(class)
library(tidyverse)
library(randomForest)
library(caret)
path3<-"/Users/jamesmcsweeney/Desktop/WildFire CSC411 Project/R SCRIPTS/dataframe.csv"
dataframe<-read.csv(path3,header=TRUE)
dataframe$STAT_CAUSE=as.factor(dataframe$STAT_CAUSE)
num_samples = dim(dataframe)[1]
train=sample(num_samples,num_samples*0.8)
#
tree.carseats=tree(STAT_CAUSE~.-Latitude-Longitude,dataframe)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty = 0, cex=0.75)
pred = predict(tree.carseats, type="class")
table(pred)
#decison tree without DAYL
# tree.carseats=tree(STAT_CAUSE~.-Latitude-Longitude-DAYMET_004_dayl,dataframe)
# summary(tree.carseats)
# plot(tree.carseats)
# text(tree.carseats, pretty = 0, cex=0.75)

#hyper parameter tunning with daylight
#using mtry sqrt(number of columns)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(1)
mtry <- sqrt(10)
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(STAT_CAUSE~.-Latitude-Longitude, data=dataframe,subset=train, method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control)
print(rf_default)
#using random
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(1)
rf_random <- train(STAT_CAUSE~.-Latitude-Longitude, data=dataframe,subset=train, method="rf", metric="Accuracy", tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)
#best model was at mtry=3
#using grid search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(1)
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(STAT_CAUSE~.-Latitude-Longitude, data=dataframe,subset=train, method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)
#best model was at mtry=3
#random forest model
set.seed(1)
BestModel1 <- randomForest(STAT_CAUSE~.-Latitude-Longitude, data = dataframe,subset=train,mtry=3, importance = TRUE)
getTree(BestModel1,1,labelVar=TRUE)
bm<-ctree(randomForest(STAT_CAUSE~.-Latitude-Longitude, data = dataframe,subset=train,mtry=3, importance = TRUE))
plot(bm, type="simple")
test_cl<-dataframe[-train,]
predictions2 <- predict(BestModel1, test_cl, type = "class")
confusionMatrix(predictions2, test_cl$STAT_CAUSE)
varImpPlot(BestModel1)

plot(BestModel1)

#hyper parameter tunning without daylight
#standard CV tuning
# control <- trainControl(method="repeatedcv", number=10, repeats=3)
# set.seed(1)
# mtry <- sqrt(10)
# tunegrid <- expand.grid(.mtry=mtry)
# rf_default <- train(STAT_CAUSE~.-DAYMET_004_dayl-Latitude-Longitude, data=DAMWithFire,subset=train, method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control)
# print(rf_default)
# #tunning through random
# control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
# set.seed(1)
# rf_random <- train(STAT_CAUSE~.-DAYMET_004_dayl-Latitude-Longitude, data=DAMWithFire,subset=train, method="rf", metric="Accuracy", tuneLength=15, trControl=control)
# print(rf_random)
# plot(rf_random)
# #tunning through a grid
# control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
# set.seed(1)
# tunegrid <- expand.grid(.mtry=c(1:15))
# rf_gridsearch <- train(STAT_CAUSE~.-DAYMET_004_dayl-Latitude-Longitude, data=DAMWithFire,subset=train, method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control)
# print(rf_gridsearch)
# plot(rf_gridsearch)
