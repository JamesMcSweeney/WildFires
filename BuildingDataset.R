library(tidyverse)

#received the date, location , and cause of fire from an SQLite database
path3<-"/Users/jamesmcsweeney/Desktop/WildFire CSC411 Project/wildfireSQL/wildfireinXL.csv"
wildfire<-read.csv(path3,header=FALSE)
colnames(wildfire)<-c("Longitude","Latitude","STAT_CAUSE","DISCOVERY_DOY")
wildfire<-wildfire%>% dplyr::select("Longitude","Latitude","STAT_CAUSE","DISCOVERY_DOY")

#used the coordinates to request wildfire Data
wild_Coordinates<-wildfire%>%dplyr::select("Longitude","Latitude")

#wildfire daymet info
wildDay<-read.csv("/Users/jamesmcsweeney/Desktop/WildFire CSC411 Project/wildfire points by date/DAYMETwildheaders.csv")
#wildfire mod13 info
wildMod<-read.csv("/Users/jamesmcsweeney/Desktop/WildFire CSC411 Project/wildfire points by date/MODWILDHEADErs.csv")
#non-Wildfire Daymet info
Day<-read.csv("/Users/jamesmcsweeney/Downloads/daymet.csv")
#non-Wildfire Modinfo
Mod<-read.csv("/Users/jamesmcsweeney/Downloads/mod13.csv")

#removed some duplicates and data that was not useable
Day<-Day%>%distinct()
Mod<-Mod%>%distinct()%>%filter(MOD13A1_006__500m_16_days_EVI>0)%>%dplyr::select(-c(MOD13A1_006__500m_16_days_VI_Quality_Land.Water_Mask_Description,MOD13A1_006__500m_16_days_VI_Quality_Possible_snow.ice_Description))
DayAndMod<-right_join(Day,Mod)

#combined all of the wildfire data into one dataset
WILDDayAndMod<-wildDay%>%full_join(wildMod,by=c("Latitude","Longitude"))
WILDDayAndMod <- WILDDayAndMod %>%add_column(STAT_CAUSE = 1)

#combined all of the non-wildfire data into one dataset
cleanWildDataAndFire<-WILDDayAndMod%>%dplyr::select(Longitude,Latitude,STAT_CAUSE,DAYMET_004_prcp,DAYMET_004_swe,DAYMET_004_vp,DAYMET_004_srad,DAYMET_004_dayl,MOD13A1_006__500m_16_days_EVI,MOD13A1_006__500m_16_days_NDVI,MOD13A1_006__500m_16_days_NIR_reflectance)
cleanWildDataAndFire<-na.omit(cleanWildDataAndFire)

#had to downsample to get the same amount of points in both Fire and Non fire datasets.This was to avoid class imbalance.
DayAndMod<-sample_n(DayAndMod,241) %>%add_column(STAT_CAUSE = 0)
#joined the Fire and non fire data into one dataframe
DAMWithFire<-full_join(DayAndMod,cleanWildDataAndFire)
#we will be classifying based off the STAT_Cause therefor it will be a factor
DAMWithFire$STAT_CAUSE=as.factor(DAMWithFire$STAT_CAUSE)


library(readr)

write_csv(DAMWithFire, "/Users/jamesmcsweeney/Desktop/WildFire CSC411 Project/R SCRIPTS/dataframe.csv")




