library(class)
library(tidyverse)
path3<-"/Users/jamesmcsweeney/Desktop/WildFire CSC411 Project/R SCRIPTS/dataframe.csv"
dataframe<-read.csv(path3,header=TRUE)


dataframe$STAT_CAUSE=as.factor(dataframe$STAT_CAUSE)


attach(dataframe)
# cbind(): column bind
train=sample(num_samples,num_samples*0.8)

# WITH daylight KNN
train.X = cbind(DAYMET_004_prcp,DAYMET_004_swe,DAYMET_004_vp,DAYMET_004_srad,DAYMET_004_dayl,MOD13A1_006__500m_16_days_EVI,MOD13A1_006__500m_16_days_NDVI,MOD13A1_006__500m_16_days_NIR_reflectance)[train,]
test.X = cbind(DAYMET_004_prcp,DAYMET_004_swe,DAYMET_004_vp,DAYMET_004_srad,DAYMET_004_dayl,MOD13A1_006__500m_16_days_EVI,MOD13A1_006__500m_16_days_NDVI,MOD13A1_006__500m_16_days_NIR_reflectance)[-train,]
train.cause = STAT_CAUSE[train]
test.cause  = STAT_CAUSE[-train]
# Ensure reproducibility of results
set.seed(1)


#hyperparameter tuning for K
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(1)
knn_fit <- train(STAT_CAUSE ~., data = dataframe,subset=train, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
knn_fit
#5 was deemed best


knn.pred = knn(train.X, test.X, train.cause, k=5)
summary(knn.pred)
table(knn.pred, test.cause )
mean(knn.pred==test.cause )
roc_auc_score(y_test, y_pred)
knn.pred = knn(train.X, test.X, train.cause, k=25)
summary(knn.pred)
table(knn.pred, test.cause )
mean(knn.pred==test.cause )



set.seed(1)
#hyperparameter tuning without Daylight
knn_fit <- train(STAT_CAUSE ~.-DAYMET_004_dayl, data = dataframe,subset=train, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
knn_fit

#without Daylight KNN
train.X = cbind(DAYMET_004_prcp,DAYMET_004_swe,DAYMET_004_vp,DAYMET_004_srad,MOD13A1_006__500m_16_days_EVI,MOD13A1_006__500m_16_days_NDVI,MOD13A1_006__500m_16_days_NIR_reflectance)[train,]
test.X = cbind(DAYMET_004_prcp,DAYMET_004_swe,DAYMET_004_vp,DAYMET_004_srad,MOD13A1_006__500m_16_days_EVI,MOD13A1_006__500m_16_days_NDVI,MOD13A1_006__500m_16_days_NIR_reflectance)[-train,]
train.cause = STAT_CAUSE[train]
test.cause = STAT_CAUSE[-train]

set.seed(1)
#9 was deemed best
knn.pred = knn(train.X, test.X, train.cause, k=9)
summary(knn.pred)
table(knn.pred, test.cause )
mean(knn.pred==test.cause )

knn.pred = knn(train.X, test.X, train.cause, k=18)
summary(knn.pred)
table(knn.pred, test.cause )
mean(knn.pred==test.cause )
