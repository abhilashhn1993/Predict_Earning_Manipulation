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

#C-Manipulater -- is the target variable with Yes or No levels.
#Change the col name
colnames(df)[11] <- "C_MANIPULATOR"

#Check for NULL values
sum(is.na(df))
#There are no null values in the dataset

#Peek at the Features 
str(df)

#Removing ID and C-MANIPULATOR columns
df$`Company ID` <- NULL
df$Manipulater <- NULL

dim(df)
#1239 rows and 9 columns

str(df)
#Variable Transformation

#Convert Manipulater into Factor
df$C_MANIPULATOR <- as.factor(df$C_MANIPULATOR)

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

#      SGAI              ACCR               LEVI         C_MANIPULATOR
# Min.   : 0.0000   Min.   :-3.14350   Min.   : 0.0000   No :1200   
# 1st Qu.: 0.8988   1st Qu.:-0.07633   1st Qu.: 0.9232   Yes:  39   
# Median : 1.0000   Median :-0.02924   Median : 1.0131              
# Mean   : 1.1072   Mean   :-0.03242   Mean   : 1.0571              
# 3rd Qu.: 1.1300   3rd Qu.: 0.02252   3rd Qu.: 1.1156              
# Max.   :49.3018   Max.   : 0.95989   Max.   :13.0586

#Statistical Tests
library(tidyverse)
library(broom)
library(pander)

a1 <- tidy(aov(DSRI~C_MANIPULATOR,  data=df))
a2 <- tidy(aov(GMI~C_MANIPULATOR,  data=df))
a3 <- tidy(aov(AQI~C_MANIPULATOR,  data=df))
a4 <- tidy(aov(SGI~C_MANIPULATOR,  data=df))
a5 <- tidy(aov(DEPI~C_MANIPULATOR,  data=df))
a6 <- tidy(aov(SGAI~C_MANIPULATOR,  data=df))
a7 <- tidy(aov(ACCR~C_MANIPULATOR,  data=df)) 
a8 <- tidy(aov(LEVI~C_MANIPULATOR,  data=df))

res=rbind(a1$p.value,a2$p.value,a3$p.value,a4$p.value,a5$p.value
          ,a6$p.value,a7$p.value,a8$p.value)

aov_res=data.frame(rbind("DSRI","GMI","AQI",
                         "SGI","DEPI","SGAI","ACCR",
                         "LEVI"))
res_tab = cbind(aov_res,res)
View(res_tab)

names(res_tab)[1] <- "Variables"
names(res_tab)[2] <- "p-values"
res_tab$`2`<- NULL

res_tab
#  Variables     p-values
#1      DSRI 1.322888e-24
#2       GMI 2.466796e-06
#3       AQI 1.972604e-06
#4       SGI 1.384494e-12
#5      DEPI 1.947399e-01
#6      SGAI 4.837263e-13
#7      ACCR 1.122713e-04
#8      LEVI 1.076081e-06

View(res_tab)
#Variable DEPI is not significant with p-value of 0.1947


########################### QUESTION 1C#############################

###################### Choosing the sample dataset #################
df_sample <- read_excel("Predict_Earning_Manipulation.xlsx", sheet = "Sample for Model Development")
dim(df_sample)
#220 rows and 11 columns
colnames(df_sample)[11] <- "C_MANIPULATOR"
#Removing ID and C-MANIPULATOR columns
df_sample$`Company ID` <- NULL
df_sample$Manipulator <- NULL
#Convert Manipulator into Factor
df_sample$C_MANIPULATOR <- as.factor(df_sample$C_MANIPULATOR)
str(df_sample)
dim(df_sample)
#220 rows and 9 columns

#################### UNDERSAMPLING THE DATASET #####################

#We use randomised undersampling technique
install.packages("ROSE")
library(ROSE)
dfs <- ovun.sample(C_MANIPULATOR ~ ., data = df_sample, method = "under", N = 100, seed = 1)$data

#creating test and train splits
split <- sample(nrow(dfs), 0.7*nrow(dfs))

dfs_train <- dfs[split,]
dfs_test <- dfs[-split,]

table(dfs_train$C_MANIPULATOR) #46 0s and 24 1s
table(dfs_test$C_MANIPULATOR) # 15 0s and 15 1s

################# STEPWISE LOGISTIC REGRESIION #####################
step_mod <- stepAIC(glm(C_MANIPULATOR~., data=dfs_train, family="binomial"))
summary(step_mod)

#Coefficients:
#               Estimate Std. Error z value Pr(>|z|)   
#(Intercept)    -7.0554     2.1684  -3.254  0.00114 **
#  DSRI          0.8285     0.2880   2.877  0.00402 **
#  GMI           1.5997     0.9153   1.748  0.08051 . 
#  AQI           0.4246     0.2938   1.445  0.14837   
#  SGI           2.2892     1.0672   2.145  0.03195 * 
#  ACCR          6.2375     2.1698   2.875  0.00404 **

#We build the model on different samples of the dataset for identifying the significant variables

#Sample 2
dfs2 <- ovun.sample(C_MANIPULATOR ~ ., data = df_sample, method = "under", N = 78, seed = 123)$data
step_mod2 <- stepAIC(glm(C_MANIPULATOR~., data=dfs2, family="binomial"))
summary(step_mod2)

#Sample 3
dfs3 <- ovun.sample(C_MANIPULATOR ~ ., data = df_sample, method = "under", N = 78, seed = 44)$data
step_mod3 <- stepAIC(glm(C_MANIPULATOR~., data=dfs3, family="binomial"))
summary(step_mod3)
#41.597

dfs4 <- ovun.sample(C_MANIPULATOR ~ ., data = df_sample, method = "under", N = 100, seed = 24)$data
step_mod4 <- stepAIC(glm(C_MANIPULATOR~., data=dfs4, family="binomial"))
summary(step_mod4)
#47.461

#From the models built using four different samples, following features are found to be significant are
#ACCR SGI AQI DSRI GMI

#THE FINAL MODEL
step_mod <- stepAIC(glm(C_MANIPULATOR~., data=dfs_train, family="binomial"))
summary(step_mod)

########################## QUESTION 1D #############################

#Predictions on test-data
pred <- predict(step_mod, newdata = dfs_test, type = "response")
pred <- ifelse(pred > 0.5, 1,0)
confusionMatrix(table(pred, dfs_test$C_MANIPULATOR))

#Accuracy : 0.8333
#Sensitivity : 1.000          
#Specificity : 0.6667

#The stepwise logistic model developed on a balanced dataset gives an accuracy of 83.3% on the test data
#It also has a Sensitivity value of 0.6667 for the chosen cut-off value of 0.5

#We plot a ROC curve to understand and evaluate the performance of the model and 
#also to identify the optimal cut-off point

install.packages("ROCR")
library(ROCR)

pr <- predict.glm(step_mod, dfs_test, type = "response")
pr <- round(pr)

pred_roc = prediction(pr, dfs_test$C_MANIPULATOR)
perf_roc = performance(pred_roc,"tpr","fpr")

#PLOTTING ROC Curve
plot(perf_roc, col = "black", lty = 3, lwd = 3)


#Trying cut-off points
#We choose different cut-off points and try to evaluate the performance of the model

#cut off 0.4
pred2 <- predict(step_mod, newdata = dfs_test, type = "response")
pred2 <- ifelse(pred2 > 0.4, 1,0)
confusionMatrix(table(pred2, dfs_test$C_MANIPULATOR))
#Accuracy 86.67%
#Sensitivity 0.9333
#Specificity 0.8

#cut-off 0.6
pred3 <- predict(step_mod, newdata = dfs_test, type = "response")
pred3 <- ifelse(pred3 > 0.6, 1,0)
confusionMatrix(table(pred3, dfs_test$C_MANIPULATOR))
#Accuracy 73.33%
#Sensitivity 1.000
#Specificity 0.4667

#cut-off 0.3
pred4 <- predict(step_mod, newdata = dfs_test, type = "response")
pred4 <- ifelse(pred4 > 0.3, 1,0)
confusionMatrix(table(pred4, dfs_test$C_MANIPULATOR))

#Accuracy 86.67%
#Sensitivity 0.8667
#Specificity 0.8667

#We see that increasing the cut-off from 0.5 to 0.6 decreased the accuracy by 10%
#We see that decreasing the cut-off from 0.5 to 0.4 increased the accuracy by 3%

#So, we choose Optimal cut-off point to be 0.4

########################## QUESTION 1E #############################

#We choose different cut off values and find the max Youden's Index

yIndex <- c()
j=1
for(i in c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)){
  predvals <- ifelse(predict(step_mod, newdata=dfs_test, type="response")>i,1,0)
  conf_mat <- table(predvals, dfs_test$C_MANIPULATOR)
  yIndex[j] <- (specificity(conf_mat) + sensitivity(conf_mat) -1)
  j=j+1
}

max(yIndex)
#0.7333 is the max value for Youden's index 
yIndex
#Max value of cut-off probability is hence 0.4

#Cost-based method
#The case study suggests that accuracy is important measure and misclassification of manipulators as 
#non-manipulators is equally alarming as misclassifying a non-manipulator as a manipulator

#Threfore, we penalize both the misclassifications with equal weights of 0.5
#Finding the cut-off probability for which the cost has the minimum value

cost <- c()
j=1
for(i in c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)){
  predvals <- ifelse(predict(step_mod, newdata=dfs_test, type="response")>i,1,0)
  cnf <- table(predvals, dfs_test$C_MANIPULATOR)
  cost[j] <- (0.5*(cnf[2,1]/(cnf[2,1] + cnf[2,2])) + 0.5*(cnf[1,2]/(cnf[1,2] + cnf[1,1])))
  j=j+1
}
min(cost)
cost
#The minimum cost is 0.125 for cut off values of 0.2 and 0.5. 
#Since, cut-off 0.5 gives better model performance, we choose cut-off value to be 0.5

########################## QUESTION 1F #############################

#In order to determine the M-score we go back to our model and the co-efficient values 
#of the significant variables
summary(step_mod)

#Coefficients:
#               Estimate     
#(Intercept)    -7.0554     
#  DSRI          0.8285     
#  GMI           1.5997    
#  AQI           0.4246     
#  SGI           2.2892    
#  ACCR          6.2375


#We determine the M-score by substituting the key predictors in a linear equation and predict the 
#resulting values. 

#For cut-off=0.5, we predict the C_MANIPULATOR values on the test data
dt <- dfs_test

#Predict on test_data 
dt$C_MANIPULATOR <- predict(step_mod, newdata = dt, type = "response")
dt$C_MANIPULATOR <- ifelse(dt$C_MANIPULATOR > 0.5, 1,0)

#We compute the m-score by creating a new feature on the test dataset by linearly combining
#the key predictors found from the model using their co-efficient and intercept values
dt$m_score <- -7.0554 + dt$DSRI*0.8285 + dt$GMI*1.5997 + dt$AQI*0.4246 + dt$SGI*2.2892 + 
              dt$ACCR*6.2375

#Now, we compare the values of m_score and the predicted C_MANIPULATOR values
summary(dt[dt[,9]==1,'m_score'])
#Min.   1st Qu.  Median    Mean   3rd Qu.    Max. 
#0.2196  0.3889  0.9037  3.7002   2.5629  22.5568
summary(dt[dt[,9]==0,'m_score'])
#    Min.  1st Qu.   Median     Mean   3rd Qu.     Max. 
#-6.89833 -2.31394 -1.55922 -1.71584  -0.91776 -0.03211

#We see the range of m_scores for both Manipulators and Non-manipulators
range(dt[dt[,9]==0,'m_score'])
# -6.89833100 -0.03210547
range(dt[dt[,9]==1,'m_score'])
# 0.2196122 22.5567557

#From the above range, we can conclude that
#Any company having a m_score > 0.22 can be considered as Manipulator.
