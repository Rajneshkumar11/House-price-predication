library(ggplot2)
library(psych)
## Warning: package 'psych' was built under R version 3.5.2
## 
## Attaching package: 'psych'
## The following objects are masked from 'package:ggplot2':
## 
##     %+%, alpha
library(faraway)
## Warning: package 'faraway' was built under R version 3.5.2
## 
## Attaching package: 'faraway'
## The following object is masked from 'package:psych':
## 
##     logit
library(lmtest)
## Loading required package: zoo
## 
## Attaching package: 'zoo'
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
#train_ur_fin = read.csv('df_transformed.csv')
#Remove unwanted columns during EDA
#train_ur_fin<-train_ur_fin[ , -which(names(train_ur_fin) %in% c("MasVnrArea"))]
train_nn =data.frame(read.csv('TransformedTrain.csv',stringsAsFactors =TRUE))
trainsd =read.csv('TransformedTrain.csv')
#Select features based on phase-1 model columns during EDA
#df_train<-train_nn[ , which(names(train_nn) %in%c("MSSubClass","LotArea",
 #                                                 "OverallQual","GrLivArea",
 #                                                 "Fireplaces","GarageArea",
 #                                                 "LotShape","Foundation","BsmtFinType1",
 #                                                 "BsmtQual","BsmtCond","KitchenQual",
 #                                                 "GarageCond","SaleCondition","SalePrice"))]
df_train<-train_nn[,-1]
set.seed(123)

index <-sample(1:nrow(df_train),round(0.70*nrow(df_train)))
train <-data.frame(df_train[index,])
test <-data.frame(df_train [-index,])
lm_model_fin =lm(train$trainsd.SalePrice~.,train)
summary(lm_model_fin)
plot(lm_model_fin)
#train_ur_fin$predict = predict(lm_model_fin,train_ur_fin)

#plot1<-ggplot(train_ur_fin,aes(x=SalePrice,y = predict)) +
#  geom_point()+
#  ggtitle("Multiple Linear Regression Prediction") +
#  labs(x = "Sale Price")+
#  labs(y = "Predicted Price")
#plot1+geom_smooth(method = lm)

#vif(lm_model_fin)
#dwtest(lm_model_fin)
#Do stepwise regression
library(MASS)

step <-stepAIC(lm_model_fin, direction="both")
step$anova # display results 
lm_model_fin =NULL
lm_model_fin=eval(step$call)

summary(lm_model_fin)
predict =predict(lm_model_fin,test)
plot1<-ggplot(test,aes(x=test$trainsd.SalePrice,y = predict)) +
  geom_point()+
  ggtitle("Multiple Linear Regression Prediction") +
  labs(x ="Sale Price")+
  labs(y ="Predicted Price")
plot1+geom_smooth(method = lm)
vif(lm_model_fin)
dwtest(lm_model_fin)
## 
##  Durbin-Watson test
## 
## data:  lm_model_fin
## DW = 1.7832, p-value = 0.0007317
## alternative hypothesis: true autocorrelation is greater than 0
# Calculating MAE, RMSE and Pearson corr R

MAE=sum(abs(test$trainsd.SalePrice-predict))
MAE
## [1] 6430930
#RMSE
RMSE=sqrt(sum((test$trainsd.SalePrice-predict)^2)/nrow(lm_model_fin$model))
RMSE
## [1] 15086.65
#Pearson correlation
pred_sp<-predict
act_sp<-test$trainsd.SalePrice
R=cor(pred_sp,act_sp,method ="pearson")
R
## [1] 0.82583
library(corrplot)
corrplot(R, method="square")
