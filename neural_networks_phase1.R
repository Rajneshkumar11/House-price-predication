library(ggplot2)
library(neuralnet)

train_nn =read.csv('train_cleaned_mlr_phase1.csv',stringsAsFactors =TRUE)
#train_nn_F=read.csv('TransformedTrain.csv',stringsAsFactors =TRUE)
#train_nn_S=read.csv('train_cleaned.csv',stringsAsFactors =TRUE)
highcorrelared <- read.csv('highlyCorrelated.csv')
#Select features based on phase-1 model columns during EDA
#train_nn<-train_nn_F[,highcorrelared]+train_nn_S$SalePrice
highcorrelared = data.matrix(rbind(highcorrelared,81))
df_nn_train<-train_nn


#Remove all NA's case
apply(df_nn_train,2,function(x) sum(is.na(x)))
##    MSSubClass       LotArea   OverallQual     GrLivArea    Fireplaces 
##             0             0             0             0             0 
##    GarageArea     SalePrice      LotShape    Foundation  BsmtFinType1 
##             0             0             0             0             0 
##      BsmtQual      BsmtCond   KitchenQual    GarageCond SaleCondition 
##             0             0             0             0             0
df_nn_train<-na.omit(df_nn_train)

#Begin NN section 
set.seed(123)

index <-sample(1:nrow(df_nn_train),round(0.70*nrow(df_nn_train)))
train <-data.frame(df_nn_train [index,])
test <-data.frame(df_nn_train [-index,])
train_descaledf <-data.frame(df_nn_train [index,])
#Recode categorical variables to numeric
train<-data.frame(sapply(train,function(x) as.numeric(x)))
test<-data.frame(sapply(test,function(x) as.numeric(x)))


# Scaling data for the NN

maxs <-as.numeric(apply(train, 2, max) )
mins <-as.numeric(apply(train, 2, min))
str(maxs)
##  num [1:15] 190 164660 10 5642 3 ...
str(mins)
##  num [1:15] 20 1300 2 438 0 160 40000 1 1 1 ...
#scaled <- data.frame(scale(train, center = mins, scale = maxs - mins))
normalize <-function(x) {
  return((x -min(x)) /(max(x) -min(x)))
}

train<-as.data.frame(lapply(train, normalize))
test<-as.data.frame(lapply(test, normalize))



n <-names(train)
f <-as.formula(paste("trainsd.SalePrice~", paste(n[!n %in% "trainsd.SalePrice"], collapse =" + ")))
nn <-neuralnet(f,data=train,hidden=c(5,3),linear.output=F,algorithm ="rprop+",threshold =0.01,
               act.fct="logistic",learningrate =0.5,rep=2)



# Visual plot of the model
plot(nn)
library(NeuralNetTools)
## Warning: package 'NeuralNetTools' was built under R version 3.5.2
#garson(nn) #only single layer
plotnet(nn)
#lekprofile(nn)#only single layer
olden(nn)
#for the whole data set inclusive non-used variables. 
#The output of the neural network, i.e. the fitted values o(x), is provided by #nn$net.result:

nn$result.matrix
# Predict
pr.nn <-compute(nn,test[,-which(names(df_nn_train) %in%c("trainsd.SalePrice"))])

pred_sp<-pr.nn$net.result
act_sp<-test$SalePrice
#Correlation between predicted and actual values
cor(pred_sp,act_sp)
##              [,1]
## [1,] 0.8267966742
# Results from NN are normalized (scaled)
# Descaling for comparison
descale=(max(train_descaledf$trainsd.SalePrice)-min(train_descaledf$trainsd.SalePrice))+min(train_descaledf$trainsd.SalePrice)
p.nn<-pr.nn$net.result*descale
test.r <-(test$trainsd.SalePrice)*descale

# Calculating MSE
MSE.nn <-sum((test.r -p.nn)^2)/nrow(test)
MSE.nn
## [1] 3372417784
RMSE<-sqrt(MSE.nn)
RMSE
## [1] 58072.52177
#Wrong formula
#RSQUARED=1-sum(test.r-pr.nn)^2/sum(test.r-mean(test.r))^2
#RSQUARED
#Get R squared from lm instead
pred<-as.data.frame(cbind(test.r,p.nn))
colnames(pred)<-c("Actual","Predicted")
str(pred)
## 'data.frame':    370 obs. of  2 variables:
##  $ Actual   : num  212043 241902 121339 106128 128100 ...
##  $ Predicted: num  279363 289872 142407 121173 160727 ...
lm_nn<-lm(Predicted~Actual,pred)
summary(lm_nn)

RMSE2=sqrt(deviance(lm_nn)/df.residual(lm_nn))
RMSE2
## [1] 36459.15342
plot1<-ggplot(pred,aes(x=Actual,y = Predicted)) +
  geom_point()+
  ggtitle("MLR of generated neuralnet prediction") +
  labs(x ="Actual")+
  labs(y ="Predicted")
plot1+geom_smooth(method = lm)

# Plot predictions /compare to above

df_nnpred<-as.data.frame(cbind(test$trainsd.SalePrice,p.nn))
colnames(df_nnpred)<-c("trainsd.SalePrice","pr.nn")
plot2<-ggplot(df_nnpred,aes(x=trainsd.SalePrice,y = p.nn)) +
  geom_point()+
  ggtitle("Generated neuralnet Prediction") +
  labs(x ="Actual")+
  labs(y ="NN Predicted")
plot2+geom_smooth(method = loess)

#Correlation between predicted and actual values
pred_sp<-p.nn
act_sp<-test$trainsd.SalePrice
#Correlation between predicted and actual values
cor(pred_sp,act_sp)
##              [,1]
## [1,] 0.8267966742
# Calculating MAE, RMSE and Pearson corr R 
MAE=sum(abs(test$trainsd.SalePrice-p.nn))
MAE
## [1] 73261673.24
#RMSE
RMSE=sqrt(sum((test$trainsd.SalePrice-p.nn)^2)/nrow(lm_nn$model))
RMSE
## [1] 208288.7363
#Pearson correlation
pred_sp<-p.nn
act_sp<-test$trainsd.SalePrice
R=cor(pred_sp,act_sp,method ="pearson")
R
##              [,1]
## [1,] 0.8267966742

#corrplot(R, method="square")
