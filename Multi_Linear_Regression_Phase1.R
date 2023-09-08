library(ggplot2)
#Read df_transformed, which created upon Explanatory Data Analysis
train_ur_fin =read.csv('TransformedTrain.csv')

features=names(train_ur_fin)
features
ncol(train_ur_fin)
trainsd =read.csv('train_cleaned.csv')
#Create Initial model
lm_model_fin =lm(trainsd$SalePrice~.,train_ur_fin)
summary(lm_model_fin)
plot(lm_model_fin)
test_ur_fin =read.csv('TransformedTest.csv')
testsd =read.csv('test_cleaned.csv')
predict=predict(lm_model_fin,train_ur_fin)
## Warning in predict.lm(lm_model_fin, train_ur_fin): prediction from a rank-
## deficient fit may be misleading
x=trainsd$SalePrice
y = predict
plot1<-ggplot(train_ur_fin[,-1],aes(x,y)) +
  geom_point()+
  ggtitle("Multiple Linear Regression Prediction") +
  labs(x ="Sale Price")+
  labs(y ="Predicted Price")
plot1+geom_smooth(method = lm)
install.packages("VIF")
library(VIF)
install.packages("lm_model_fin")
library(faraway)
## Warning: package 'faraway' was built under R version 3.5.2
vif(lm_model_fin)
install.packages("lmtest")
library(lmtest)
dwtest(lm_model_fin)

#Do stepwise regression
library(MASS)

step <-stepAIC(lm_model_fin, direction="both")

step$anova # display results 
 
lm_model_fin=eval(step$call)

summary(lm_model_fin)

vif(lm_model_fin)

dwtest(lm_model_fin)

MLR_PHASE1Train<-lm_model_fin[["model"]]
write.csv(MLR_PHASE1Train, file = "train_cleaned_mlr_phase1.csv", row.names = F)
#write.csv(cleanTest, file = "test_cleaned.csv", row.names = F)


