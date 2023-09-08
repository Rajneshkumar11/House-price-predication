library(ggplot2)
library(psych)
library (plyr)
library(MASS)
library(data.table)
library(reshape)
library(reshape2)

library(Hmisc, quietly=TRUE)
require(dplyr)
require(Amelia)
library(fBasics, quietly=TRUE)
require(mice)
library(caret)
require(dummies)
require(ggplot2)
require(scales)
require(e1071)
require(corrplot)
require(vcd)
require(VIM)
library(nnet)
library(Metrics)
library(sqldf)
library(e1071)
#-------------Data Acquisition------------------------
train<-read.csv("Train_cleaned.csv")
test<-read.csv("Test_cleaned.csv")

#------------Description Analysis--------------------

#----------SalePrice Statistication Analysis----------------------
# Get the fitted parameters used by the function
fit <- fitdistr(train$SalePrice, "normal")
class(fit)
para <- fit$estimate
print(paste(para))
train_skewness<-skewness(train$SalePrice)
train_kurtosis<-kurtosis(train$SalePrice) 
print(paste(train_skewness,train_skewness))
#z <- (72 - mu) / sigma
#Histogram
h <- hist(train$SalePrice, breaks = 20, density = 20,
          col = "lightgray",  main = "SalesPrice Normalize Curve Fitting") 
xfit <- seq(min(train$SalePrice), max(train$SalePrice), length = 40) 
yfit <- dnorm(xfit, para[1], para[2]) 
yfit <- yfit * diff(h$mids[1:2]) * length(train$SalePrice) 
lines(xfit, yfit, col = "red", lwd = 2)
#----------Multivariable Statistication Analysis----------------------
#class
train_class = class(train)
test_class=class(test)
#Dimention
train_dim = dim(train)
test_dim = dim(test)
#Summary
train_summary=summary(train)
test_summary=summary(test)
#Head
train_head = head(train)
test_head = head(test)
# Checking Categorical Data # Checking Numerical Data
train_categoricaldata<-data.frame(train)
train_colsums<-colSums(sapply(train,is.na))
train_contents<-contents(train)
print(paste(train_categoricaldata))
print(paste(train_colsums))
print(paste(train_contents))
test_categoricaldata<-data.frame(test)
test_colsums<-colSums(sapply(test,is.na))
test_contents<-contents(test)
print(paste(test_categoricaldata))
print(paste(test_colsums))
print(paste(test_contents))
#Look at Missing Values
train_structure =str(train)
missmap(train)
test_structure =str(test)
missmap(test)
### Create the dummy variable to combine the data using rbind
test$SalePrice=0
summary(test$SalePrice)
### Combining the data 
train_data=rbind(train,test)

missmap(train_data)
colSums(sapply(train_data,is.na))
contents(train_data)
### Handling the NA Values 

aggr_plot1 <- aggr(train_data[1:15], col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), 
                   cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

##### Data Having only 4 missing Values, % of Missing Values is 0.0013.
##### So we can replace with mode(most often) value.
describe(train_data$MSZoning)
train_data$MSZoning[is.na(train_data$MSZoning)]="RL"
train_data$MSZoning<-as.factor(train_data$MSZoning)

### Data Having 93% of NA'S, but replacing the values as "Noalley"
describe(train_data$Alley)
train_data$Alley= as.character(train_data$Alley)
train_data$Alley[is.na(train_data$Alley)]="Noalley"
train_data$Alley = as.factor(train_data$Alley)

##### Data Having only 2 missing Values, % of Missing Values is 0.0006851662.
##### So we can replace with mode(most often) value.
describe(train_data$Utilities)
train_data$Utilities[is.na(train_data$Utilities)]="AllPub"
is.factor(train_data$Utilities)

#### Data Having 24 missing Values, total % of Missing Values is 0.008.
##### So we can replace with mode(most often) value.
describe(train_data$MasVnrType)
train_data$MasVnrType[which(is.na(train_data$MasVnrType))]="None"
summary(train_data$MasVnrType)
is.factor(train_data$MasVnrType)
describe(train_data$MasVnrArea)
#### Data Having 23 missing Values, total % of Missing Values is 0.008,
####  AND also data having extrem outliers, so we are replacing the values to Median
ggplot(train_data, aes(y=MasVnrArea)) +
  geom_boxplot(aes(x="All"), notch=TRUE, fill="grey") +
  stat_summary(aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggtitle("Distribution of MasVnrArea") +
  theme(legend.position="none")
describe(train_data$MasVnrArea)
train_data$MasVnrArea[which(is.na(train_data$MasVnrArea))]= median(train_data$MasVnrArea,na.rm=TRUE)


describe(train_data$Exterior1st)
### Data having only one Missing Values, total % of Missing Values is 0.0003425831.
##### So we can replace with mode(most often) value.
train_data$Exterior1st[which(is.na(train_data$Exterior1st))]="VinylSd"

describe(train_data$Exterior2nd)
### Data having only one Missing Values, total % of Missing Values is 0.0003425831. 
##### So we can replace with mode(most often) value.
train_data$Exterior2nd[which(is.na(train_data$Exterior2nd))]="VinylSd"


colSums(sapply(train_data,is.na))
describe(train_data$BsmtCond)
### As per the Project discription NA values are replaced to "NoBasement"
train_data$BsmtCond = as.character(train_data$BsmtCond)
train_data$BsmtCond[which(is.na(train_data$BsmtCond))]="NoBasement"
train_data$BsmtCond = as.factor(train_data$BsmtCond)
describe(train_data$BsmtExposure)
### As per the Project discription NA values are replaced to "NoBasement"
train_data$BsmtExposure = as.character(train_data$BsmtExposure)
train_data$BsmtExposure[which(is.na(train_data$BsmtExposure))]="NoBasement"
train_data$BsmtExposure = as.factor(train_data$BsmtExposure)
describe(train_data$BsmtQual)
### As per the Project discription NA values are replaced to "NoBasement"
train_data$BsmtQual = as.character(train_data$BsmtQual)
train_data$BsmtQual[which(is.na(train_data$BsmtQual))]="NoBasement"
train_data$BsmtQual = as.factor(train_data$BsmtQual)
escribe(train_data$BsmtFinType2)
### As per the Project discription NA values are replaced to "NoBasement"
train_data$BsmtFinType2 = as.character(train_data$BsmtFinType2)
train_data$BsmtFinType2[which(is.na(train_data$BsmtFinType2))]="NoBasement"
train_data$BsmtFinType2 = as.factor(train_data$BsmtFinType2)
describe(train_data$BsmtFinType1)
### As per the Project discription NA values are replaced to "NoBasement"
train_data$BsmtFinType1 = as.character(train_data$BsmtFinType1)
train_data$BsmtFinType1[which(is.na(train_data$BsmtFinType1))]="NoBasement"
train_data$BsmtFinType1 = as.factor(train_data$BsmtFinType1)

describe(train_data$BsmtFinSF1)
### Data Having 1 missing Values, Replaced NA using median value

# ggplot(train_data, aes(y=BsmtFinSF1)) +
#   geom_boxplot(aes(x="All"), notch=TRUE, fill="grey") +
#   stat_summary(aes(x="All"), fun.y=mean, geom="point", shape=8) +
#   ggtitle("Distribution of BsmtFinSF1") +
#   theme(legend.position="none")

train_data$BsmtFinSF1[which(is.na(train_data$BsmtFinSF1))]=median(train_data$BsmtFinSF1,na.rm=TRUE)
describe(train_data$BsmtFinSF2)
### Data Having 1 missing Values, Replaced NA using median value
train_data$BsmtFinSF2[which(is.na(train_data$BsmtFinSF2))]=median(train_data$BsmtFinSF2,na.rm=TRUE)

describe(train_data$BsmtUnfSF)
### Data Having 1 missing Values, Replaced NA using median value
train_data$BsmtUnfSF[which(is.na(train_data$BsmtUnfSF))]=median(train_data$BsmtUnfSF,na.rm=TRUE)

describe(train_data$TotalBsmtSF)
### Data Having 1 missing Values, Replaced NA using median value
train_data$TotalBsmtSF[which(is.na(train_data$TotalBsmtSF))]=median(train_data$TotalBsmtSF,na.rm=TRUE)


colSums(sapply(train_data,is.na))
describe(train_data$Electrical)
### Data Having 1 missing Values, Replaced NA using mode value
train_data$Electrical[which(is.na(train_data$Electrical))]="SBrkr"
is.factor(train_data$Electrical)


describe(train_data$BsmtFullBath)
### Data Having 2 missing Values, Replaced NA using median value
train_data$BsmtFullBath[which(is.na(train_data$BsmtFullBath))]=median(train_data$BsmtFullBath,na.rm=TRUE)

describe(train_data$BsmtHalfBath)
### Data Having 2 missing Values, Replaced NA using median value
train_data$BsmtHalfBath[which(is.na(train_data$BsmtHalfBath))]=median(train_data$BsmtHalfBath,na.rm=TRUE)



describe(train_data$FireplaceQu)
### As per the Project discription NA values are replaced to "No Fireplace"
train_data$FireplaceQu=as.character(train_data$FireplaceQu)
train_data$FireplaceQu[which(is.na(train_data$FireplaceQu))]="NoFireplace"
train_data$FireplaceQu=as.factor(train_data$FireplaceQu)
describe(train_data$GarageYrBlt)
### Identified some Noise data in GarageYrBlt, We found year as 2207 and replaced to 2007
### total 159 values are missing. the % of missing values is 0.06
train_data$GarageYrBlt[which(train_data$GarageYrBlt==2207)]=2007

describe (train_data$GarageFinish)
### As per the Project Discription NA values are replaced to No Grage
train_data$GarageFinish=as.character(train_data$GarageFinish)
train_data$GarageFinish[which(is.na(train_data$GarageFinish))]="NoGarage"
train_data$GarageFinish=as.factor(train_data$GarageFinish)
describe (train_data$GarageQual)
### As per the Project Discription NA values are replaced to No Grage
train_data$GarageQual=as.character(train_data$GarageQual)
train_data$GarageQual[which(is.na(train_data$GarageQual))]="NoGarage"
train_data$GarageQual=as.factor(train_data$GarageQual)

describe (train_data$GarageCond)
### As per the Project Discription NA values are replaced to No Grage
train_data$GarageCond=as.character(train_data$GarageCond)
train_data$GarageCond[which(is.na(train_data$GarageCond))]="NoGarage"
train_data$GarageCond=as.factor(train_data$GarageCond)

describe (train_data$GarageType)
### As per the Project Discription NA values are replaced to No Grage
train_data$GarageType=as.character(train_data$GarageType)
train_data$GarageType[which(is.na(train_data$GarageType))]="NoGarage"
train_data$GarageType=as.factor(train_data$GarageType)


describe(train_data$Functional)
#### Data Having 2 missing Values. Replaced with mode values
train_data$Functional[which(is.na(train_data$Functional))]="Typ"

describe(train_data$KitchenQual)
#### Data Having 1 missing Values. Replaced with Mode Values
train_data$KitchenQual[which(is.na(train_data$KitchenQual))]= "TA"
describe(train_data$GarageCars)
### Data Having 1 Missing Values, replaced with Mode values
train_data$GarageCars[which(is.na(train_data$GarageCars))]=2

describe(train_data$GarageArea)
### Data Having Missing Values,so we replaced with median values
train_data$GarageArea[which(is.na(train_data$GarageArea))]=median(train_data$GrLivArea,na.rm=T)

colSums(sapply(train_data,is.na))

describe(train_data$PoolQC)
### As per the Project Discription NA values are replaced to No pool.
train_data$PoolQC=as.character(train_data$PoolQC)
train_data$PoolQC[which(is.na(train_data$PoolQC))]="NoPool"
train_data$PoolQC=as.factor(train_data$PoolQC)

describe(train_data$MiscFeature)
### As per the Project Discription NA values are replaced to None
train_data$MiscFeature=as.character(train_data$MiscFeature)
train_data$MiscFeature[which(is.na(train_data$MiscFeature))]="None"
train_data$MiscFeature=as.factor(train_data$MiscFeature)

describe(train_data$Fence)
### As per the Project Discription NA values are replaced to NO Fance
train_data$Fence=as.character(train_data$Fence)
train_data$Fence[which(is.na(train_data$Fence))]="NoFence"
train_data$Fence=as.factor(train_data$Fence)

describe(train_data$SaleType)
#### Data Having only 1 missing values. replaced to mean
train_data$SaleType[which(is.na(train_data$SaleType))]="WD"
is.factor(train_data$SaleType)

colSums(sapply(train_data,is.na))
#### LotFrontage and GarageYrblt is Major missing Values in data
#### and can be replaced with bagImpute using Propress
library(ipred)
train_data_preprocess = preProcess(train_data[,c("LotFrontage","GarageYrBlt")],method="bagImpute")
summary(train_data_preprocess) 
train_data_pred= predict(train_data_preprocess,train_data[,c("LotFrontage","GarageYrBlt")],type="class")
train_data$LotFrontage = train_data_pred$LotFrontage
train_data$GarageYrBlt = train_data_pred$GarageYrBlt

colSums(sapply(train_data,is.na))
# Spilt to Train and Test Data
cleanTrain<-sqldf("select * from train_data where id < 1461")
cleanTest<-sqldf("select * from train_data where id > 1460")
# STEP 2 : Model Fitting and predicting 1 Using Linear regression ALL variables
lm_model<-lm(SalePrice~., data=cleanTrain)

summary(lm_model)
pred <- predict(lm_model, cleanTest)


#Prediction using lm with ALL variables
lm_submit_1 <- data.frame(Id = test$Id, SalePrice = pred)
write.csv(cleanTrain, file = "train_cleaned.csv", row.names = F)
write.csv(cleanTest, file = "test_cleaned.csv", row.names = F)
