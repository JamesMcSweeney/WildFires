library(class)
library(tidyverse)
library(randomForest)
library(caret)
library(tree)


path3<-"/Users/jamesmcsweeney/Desktop/WildFire CSC411 Project/R SCRIPTS/dataframe.csv"
dataframe<-read.csv(path3,header=TRUE)
dataframe$STAT_CAUSE=as.factor(dataframe$STAT_CAUSE)
dt = sort(sample(nrow(dataframe), nrow(dataframe)*.7))
train<-dataframe[dt,]
test<-dataframe[-dt,]

#Classifcation Tree
demoTree=tree(STAT_CAUSE~.-Latitude-Longitude,train)
#plotting the tree
plot(demoTree)
text(demoTree, pretty = 0, cex=0.75)
#predicting on the tree
pred = predict(demoTree,test, type="class")
table(test$STAT_CAUSE,pred)
#preforming Cross validation
cv_tree=cv.tree(demoTree, FUN = prune.misclass)
prune_tree = prune.misclass(demoTree,best=3)
pred = predict(prune_tree,test, type="class")
table(test$STAT_CAUSE,pred)


#random Forest
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(1)
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(STAT_CAUSE~.-Latitude-Longitude, data=train, method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
set.seed(1)

BestModel1 <- randomForest(STAT_CAUSE~.-Latitude-Longitude, data = train,mtry=3, importance = TRUE)
predictions2 <- predict(BestModel1, test, type = "class")
confusionMatrix(predictions2, test$STAT_CAUSE)


#KNN
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(1)
knn_fit <- train(STAT_CAUSE ~.-Latitude-Longitude, data = train, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
knn_fit

train.X = train%>%dplyr::select(DAYMET_004_prcp,DAYMET_004_swe,DAYMET_004_vp,DAYMET_004_srad,DAYMET_004_dayl,MOD13A1_006__500m_16_days_EVI,MOD13A1_006__500m_16_days_NDVI,MOD13A1_006__500m_16_days_NIR_reflectance)
test.X = test%>%dplyr::select(DAYMET_004_prcp,DAYMET_004_swe,DAYMET_004_vp,DAYMET_004_srad,DAYMET_004_dayl,MOD13A1_006__500m_16_days_EVI,MOD13A1_006__500m_16_days_NDVI,MOD13A1_006__500m_16_days_NIR_reflectance)
train.cause = train$STAT_CAUSE
test.cause = test$STAT_CAUSE
knn.pred = knn(train.X, test.X, train.cause, k=11)
table(knn.pred,test.cause )


#SVM

ctrl <- trainControl(method="repeatedcv",   # 10fold cross validation
                     repeats=5,         # do 5 repetitions of cv
                     summaryFunction=twoClassSummary, 
                     classProbs=TRUE)
set.seed(1)
tune.out=tune(svm,STAT_CAUSE ~ .-Latitude-Longitude, data = train,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)),trControl=ctrl)
bestmod=tune.out$best.model
summary(bestmod)


classifier = svm(STAT_CAUSE ~ .-Latitude-Longitude, data = train, kernel = "linear", cost = 10, scale = FALSE)
ypred=predict(classifier,test)
table(predict=ypred, truth=test$STAT_CAUSE)

#Naive Bayes
train_control <- trainControl(method = "cv",
                              number = 10)
# Fitting Naive Bayes Model
# to training dataset
set.seed(120) # Setting Seed
classifier_cl <- naiveBayes(STAT_CAUSE ~ .-Latitude-Longitude, data = train,trControl=train_control)


# Predicting on test data'
y_pred <- predict(classifier_cl, test)

# Confusion Matrix
cm <- table(y_pred,test$STAT_CAUSE)
cm
 
