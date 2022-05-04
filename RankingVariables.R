library(tidyverse)
library(rsample)
path3<-"/Users/jamesmcsweeney/Desktop/WildFire CSC411 Project/R SCRIPTS/dataframe.csv"
dataframe<-read.csv(path3,header=TRUE)


dataframe$STAT_CAUSE=as.logical(dataframe$STAT_CAUSE)
num_samples = dim(DAMWithFire)[1]
train=sample(num_samples,num_samples*0.8)

lm.fit1 = lm(STAT_CAUSE~.-Latitude-Longitude,data=dataframe, subset =train)
summary(lm.fit1)
lm.probs = predict(lm.fit1,dataframe, type="response")
plot(lm.probs)
table(lm.probs,dataframe$STAT_CAUSE)
mean(lm.probs)

lm.fit1 = lm(STAT_CAUSE~.-Latitude-Longitude-DAYMET_004_dayl,data=dataframe, subset =train)
summary(lm.fit1)
#i am pretty sure that this was the cleaning process, i made coordinates then checked if they were on land and then got the info from the land ones and removes non land
lm.probs = predict(lm.fit1,dataframe, type="response")

