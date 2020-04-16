library(aod)
library(ggplot2)
library(tidyverse)
library (ROCR)
library(caret)
library(readxl)
library(dplyr)

df <- read_excel("Predict_Earning_Manipulation.xlsx", sheet = "Complete Data")

View(df)

######################## STRUCTURE OF DATASET ######################

dim(df)
#1239 rows and 11 columns

#Manipulater -- is the target variable with Yes or No levels.


#Check for NULL values
sum(is.na(df))
#There are no null values in the dataset

#Peek at the Features 
str(df)

#Removing ID and C-MANIPULATOR columns
df$`Company ID` <- NULL
df$`C-MANIPULATOR` <- NULL

dim(df)
#1239 rows and 9 columns

str(df)
#Variable Transformation

#Convert Manipulater into Factor
df$Manipulater <- as.factor(df$Manipulater)

str(df)

#Summary statsitic
summary(df)

#      DSRI              GMI                AQI                SGI                DEPI        
# Min.   : 0.0000   Min.   :-20.8118   Min.   :-32.8856   Min.   : 0.02769   Min.   :0.06882  
# 1st Qu.: 0.8908   1st Qu.:  0.9271   1st Qu.:  0.7712   1st Qu.: 0.97021   1st Qu.:0.93690  
# Median : 1.0227   Median :  1.0000   Median :  1.0040   Median : 1.08896   Median :1.00191  
# Mean   : 1.1691   Mean   :  0.9879   Mean   :  0.9978   Mean   : 1.12709   Mean   :1.04014  
# 3rd Qu.: 1.1925   3rd Qu.:  1.0580   3rd Qu.:  1.2163   3rd Qu.: 1.19998   3rd Qu.:1.08136  
# Max.   :36.2912   Max.   : 46.4667   Max.   : 52.8867   Max.   :13.08143   Max.   :5.39387  

#      SGAI              ACCR               LEVI         Manipulater
# Min.   : 0.0000   Min.   :-3.14350   Min.   : 0.0000   No :1200   
# 1st Qu.: 0.8988   1st Qu.:-0.07633   1st Qu.: 0.9232   Yes:  39   
# Median : 1.0000   Median :-0.02924   Median : 1.0131              
# Mean   : 1.1072   Mean   :-0.03242   Mean   : 1.0571              
# 3rd Qu.: 1.1300   3rd Qu.: 0.02252   3rd Qu.: 1.1156              
# Max.   :49.3018   Max.   : 0.95989   Max.   :13.0586


