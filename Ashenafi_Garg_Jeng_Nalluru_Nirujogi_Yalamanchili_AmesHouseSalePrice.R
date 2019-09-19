############# Loading the dependencies ################
library("gridExtra")
library("corrplot")
library("e1071")
library("gridExtra")
library("dummies")
library("caret")
library("randomForest")
library("tidyverse")
library("Metrics")
library("gbm")
library("xgboost")
library("glmnet")
library("Boruta")
library("gbm")
library("faraway")


############ Predicting the House Price #################

#Importing the data.

train<- read.csv("/Users/hemanthnirujogi/Documents/OR568/OR568_Project/train.csv",header = T,stringsAsFactors = F)
test<- read.csv("/Users/hemanthnirujogi/Documents/OR568/OR568_Project/test.csv",header = T,stringsAsFactors = F )


dim(train)                                       
dim(test)
###Removing Outliers
options(scipen = 9999)
ggplot(train,aes(x = GrLivArea,y = SalePrice)) +
  geom_point(col = "red")
train[which(train$GrLivArea >4000 & train$SalePrice < 200000), ]$GrLivArea <- mean(hprice$GrLivArea)

###Transforming dependent variable
ggplot(train,aes(SalePrice))+geom_histogram(fill="steelblue",color="black")
ggplot(train,aes(SalePrice))+geom_histogram(fill="steelblue",color="black")+scale_x_log10()
train$SalePrice <- log(train$SalePrice+1)
test$SalePrice <- as.numeric(0)

###Binding the train and test
hprice <- rbind(train,test)
dim(hprice)
hprice = hprice[ ,-1]


options(scipen = 99)
train[1:46] %>%
  keep(is.numeric) %>%
  gather() %>%                             
  ggplot(aes(x = value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_histogram(fill = "blue") 

train[46:81] %>%
  keep(is.numeric) %>%
  gather() %>%                             
  ggplot(aes(x = value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_histogram(fill = "blue") 
  ##Transforming variables which are categorical

hprice$MSSubClass <- as.character(hprice$MSSubClass)
hprice$OverallQual <- as.character(hprice$OverallQual)
hprice$OverallCond <- as.character(hprice$OverallCond)

#Structure of the data set
str(hprice)
#Summary of dataset
summary(hprice)

##checking Columns which have  NA'S

missing_data <- as.data.frame(sort(sapply(hprice, function(x) sum(is.na(x))),decreasing = T))

missing_data <- (missing_data/2919)*100
colnames(missing_data)[1] <- "missingvaluesPercentage"
missing_data$features <- rownames(missing_data)
ggplot(missing_data[missing_data$missingvaluesPercentage >5,],aes(reorder(features,-missingvaluesPercentage),missingvaluesPercentage,fill= features)) +
  geom_bar(stat="identity") +theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") + ylab("Percentage of missingvalues") +
  xlab("Feature") + ggtitle("Understanding Missing Data")

ggplot(hprice,aes(x = Fence)) +
  geom_bar(fill = "blue")

#Imputing Missing Values

hprice$PoolQC[is.na(hprice$PoolQC)] = 'None'
hprice$MiscFeature[is.na(hprice$MiscFeature)] = 'None'
hprice$Alley[is.na(hprice$Alley)] = 'None'
hprice$Fence[is.na(hprice$Fence)] = 'None'
hprice$FireplaceQu[is.na(hprice$FireplaceQu)] = 'None'

#####taking care of  NA's - garage 
hprice$GarageCond[is.na(hprice$GarageCond)] = 'None'
hprice$GarageFinish[is.na(hprice$GarageFinish)] = 'None'
hprice$GarageQual[is.na(hprice$GarageQual)] = 'None'
hprice$GarageType[is.na(hprice$GarageType)] = 'None'


######taking care of NA's - Bsmt 
hprice$BsmtCond[is.na(hprice$BsmtCond)] = 'None'
hprice$BsmtExposure[is.na(hprice$BsmtExposure)] = 'None'
hprice$BsmtQual[is.na(hprice$BsmtQual)] = 'None'
hprice$BsmtFinType1[is.na(hprice$BsmtFinType1)] = 'None'
hprice$BsmtFinType2[is.na(hprice$BsmtFinType2)] = 'None'

###Masonry
hprice$MasVnrType[is.na(hprice$MasVnrType)] = 'None'
hprice$MasVnrArea[is.na(hprice$MasVnrArea)] = 0

### Looking into Lot Frontage - Linear feet of street connected to property which has 486 NA's:
##Imputing all the NA's with median value

hprice$LotFrontage[is.na(hprice$LotFrontage)] = 0 

###Garage year built
yblt <- which(is.na(hprice$GarageYrBlt))
hprice[yblt, 'GarageYrBlt'] <- hprice[yblt, 'YearBuilt']


#removing rest of the cols which has NA's

table(hprice$BsmtFullBath)
table(hprice$BsmtHalfBath)
table(hprice$MSZoning)
table(hprice$Utilities)
table(hprice$Functional)
table(hprice$Exterior1st)
table(hprice$Exterior2nd)
table(hprice$Electrical)
table(hprice$KitchenQual)
table(hprice$SaleType)
table(hprice$GarageCars)


hprice$BsmtFullBath[is.na(hprice$BsmtFullBath)] = 0
hprice$BsmtHalfBath[is.na(hprice$BsmtHalfBath)] = 0
hprice$MSZoning[is.na(hprice$MSZoning)] = 'RL'
hprice$Utilities[is.na(hprice$Utilities)] = 'AllPub'
hprice$Functional[is.na(hprice$Functional)] = 'Typ'
hprice$Exterior1st[is.na(hprice$Exterior1st)] = 'MetalSd'
hprice$Exterior2nd[is.na(hprice$Exterior2nd)] = 'VinylSd'
hprice$Electrical[is.na(hprice$Electrical)] = 'SBrkr'
hprice$KitchenQual[is.na(hprice$KitchenQual)] = 'TA'
hprice$SaleType[is.na(hprice$SaleType)] = 'WD'
hprice$GarageCars[is.na(hprice$GarageCars)] = 2
hprice$BsmtFinSF1[is.na(hprice$BsmtFinSF1)] <- 0
hprice$BsmtFinSF2[is.na(hprice$BsmtFinSF2)] <- 0
hprice$BsmtUnfSF[is.na(hprice$BsmtUnfSF)] <- 0
hprice$TotalBsmtSF[is.na(hprice$TotalBsmtSF)] <- 0 
hprice$GarageArea[is.na(hprice$GarageArea)] <- 0

sum(is.na(hprice))

#################


###Recode ordered factors as pseudo-continuous numerical variables -
hprice$ExterQual<- recode(hprice$ExterQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
hprice$ExterCond<- recode(hprice$ExterCond,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
hprice$BsmtQual<- recode(hprice$BsmtQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
hprice$BsmtCond<- recode(hprice$BsmtCond,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
hprice$BsmtExposure<- recode(hprice$BsmtExposure,"None"=0,"No"=1,"Mn"=2,"Av"=3,"Gd"=4)
hprice$BsmtFinType1<- recode(hprice$BsmtFinType1,"None"=0,"Unf"=1,"LwQ"=2,"Rec"=3,"BLQ"=4,"ALQ"=5,"GLQ"=6)
hprice$BsmtFinType2<- recode(hprice$BsmtFinType2,"None"=0,"Unf"=1,"LwQ"=2,"Rec"=3,"BLQ"=4,"ALQ"=5,"GLQ"=6)
hprice$HeatingQC<- recode(hprice$HeatingQC,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
hprice$KitchenQual<- recode(hprice$KitchenQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
hprice$Functional<- recode(hprice$Functional,"None"=0,"Sev"=1,"Maj2"=2,"Maj1"=3,"Mod"=4,"Min2"=5,"Min1"=6,"Typ"=7)
hprice$FireplaceQu<- recode(hprice$FireplaceQu,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
hprice$GarageFinish<- recode(hprice$GarageFinish,"None"=0,"Unf"=1,"RFn"=2,"Fin"=3)
hprice$GarageQual<- recode(hprice$GarageQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
hprice$GarageCond<- recode(hprice$GarageCond,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
hprice$PoolQC<- recode(hprice$PoolQC,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
hprice$Fence<- recode(hprice$Fence,"None"=0,"MnWw"=1,"GdWo"=2,"MnPrv"=3,"GdPrv"=4)

hprice$TotalSF = hprice$TotalBsmtSF + hprice$X1stFlrSF + hprice$X2ndFlrSF

summary(hprice)

library("PreProcess")
library("kernlab")
library("oompaBase")
library("Metrics")
library("caret")
library("ggplot2")
library("lattice")
library("AppliedPredictiveModeling")
library("e1071")
library("ipred")
library("MASS")
library("dummies")

#######Encoding for Categorical variable
hpricedummy <- dummy.data.frame(hprice,dummy.classes = "character")
hpricedummy <- rename(hpricedummy,"MSZoningC"="MSZoningC (all)")
hpricedummy <- rename(hpricedummy,"RoofMatlTarGrv"="RoofMatlTar&Grv")
hpricedummy <- rename(hpricedummy,"Exterior1stWdSdng"="Exterior1stWd Sdng")
hpricedummy <- rename(hpricedummy,"Exterior2ndBrkCmn"="Exterior2ndBrk Cmn")
hpricedummy <- rename(hpricedummy,"Exterior2ndWdSdng"="Exterior2ndWd Sdng")
hpricedummy <- rename(hpricedummy,"Exterior2ndWdShng"="Exterior2ndWd Shng")

str(hpricedummy)
dim(hpricedummy)

###########Transforming Skewed Variables#############

numnames <- names(which(sapply(hpricedummy,is.numeric)))
catname <- names(which(sapply(hpricedummy,is.character)))
numnam <- names(which(sapply(hpricedummy,is.numeric)))

skew <- sapply(numnames,function(x){skewness(hpricedummy[[x]],na.rm = T)})
# Let us determine a threshold skewness and transform all variables above the treshold.
skew <- skew[skew > 0.75]
# transform excessively skewed features with log(x + 1)
for(x in names(skew)) {
 hpricedummy[[x]] = log(hpricedummy[[x]]+1)
}

##################Train & Test#######################
train1 <- hpricedummy[1:1460,]
test1 <- hpricedummy[1461:2919, ]
dim(train1)

###########Linear Regression##############0.141######
trainctrl = trainControl(method = "repeatedcv",number = 10, repeats = 5 ,
                         verboseIter = T)

lmmodel = train(SalePrice ~ .,train1,method = "lm",
                trControl = trainControl(method = "cv",number = 5,verboseIter = T), na.action=na.exclude)
summary(lmmodel)

lmmodel1 = lm(SalePrice ~ .,train1)
plot(lmmodel1)
lmpred = exp(predict(lmmodel,test1))

lmsubmission = data.frame(Id = test$Id,SalePrice = lmpred)
write.csv(lmsubmission,"lmsubmission.csv",row.names = F)

#########################lm - pca ####### 0.13796##########

lmpcamodel = train(SalePrice ~ .,train1,method = "lm",trControl = trainControl(method = "cv",number = 10, repeats = 5,
                                                                               verboseIter = T),
                   preProcess = c("zv","center","scale"), na.action=na.exclude)


summary(lmpcamodel)
lmpcapred = exp(predict(lmpcamodel,test1))

lmpcasubmission = data.frame(Id = test$Id,SalePrice = lmpcapred)
write.csv(lmpcasubmission,"lmpcasubmission.csv",row.names = F)


####################Random Forest##########0.144#####
set.seed(123)
rfmodel = train(SalePrice ~ .,train1,tuneLength = 5,method = "ranger",
                trControl = trainControl(method = "cv",number = 5,
                                         verboseIter = T), na.action=na.exclude)
plot(rfmodel)
summary(rfmodel)

rfpred = exp(predict(rfmodel,test1)) - 1

rfsubmission = data.frame(Id = test$Id,SalePrice = rfpred)
write.csv(rfsubmission,"rfsubmission.csv",row.names = F)

################GLMNET MOdels#############0.1316###
set.seed(1234)
glmnetmodel = train(SalePrice ~ .,train1,tuneGrid = expand.grid(alpha = 0:1,lambda = seq(0.0001,1,length = 10)),
                    method = "glmnet",metric = "RMSE",trControl = trainctrl, na.action=na.exclude)
plot(glmnetmodel)

glmnetpred = exp(predict(glmnetmodel,test1)) - 1

glmnetsubmission = data.frame(Id = test$Id,SalePrice = glmnetpred)
write.csv(glmnetsubmission,"glmnetsubmission.csv",row.names = F)

####################GBM###################

set.seed(12)
gbmodel = train(SalePrice ~ .,train1,
                method = "gbm",metric = "RMSE",trControl = trainctrl, na.action=na.exclude)
gbmpred = exp(predict(gbmodel,test1))

gbmsubmission = data.frame(Id = test$Id,SalePrice = gbmpred)
write.csv(gbmsubmission,"gbmsubmission.csv",row.names = F)
names(train1)

##################   XGBOOST    #######################################

dtrain = xgb.DMatrix(data = as.matrix(train1[ ,-266]),label = as.matrix(train1$SalePrice))
dtest = xgb.DMatrix(data = as.matrix(test1[ ,-266]),label = as.matrix(test1$SalePrice))

xgb <- xgboost(booster="gbtree",data = dtrain, nfold = 5,nrounds = 2500, verbose = FALSE, 
               objective = "reg:linear", eval_metric = "rmse", nthread = 8, eta = 0.01, 
               gamma = 0.0468, max_depth = 6, min_child_weight = 1.41, subsample = 0.769, colsample_bytree =0.283)

xgbpred = exp(predict(xgb,dtest))-1
xgbsubmission =  data.frame(Id = test$Id,Saleprice = xgbpred)
write.csv(xgbsubmission,"xgbsubmission.csv",row.names = F)

####################    weighted average     ##########0.12834################

weightsubmission = data.frame(Id = test$Id,SalePrice = (0.6*glmnetpred + 0.4*gbmpred))
write.csv(weightsubmission,"weightsubmission10.csv",row.names = F)
weightsubmission$results
summary(weightsubmission)
#####################Including only Variable############
set.seed(12)
gbimpmodel = train(SalePrice ~ TotalSF+GrLivArea+ ExterQual+ KitchenQual + FireplaceQu +
                     BsmtQual+  GarageCars+YearBuilt+ GarageArea+GarageFinish+
                     LotArea+YearRemodAdd +BsmtFinSF1+X1stFlrSF+ TotalBsmtSF +
                     MSZoningC+ Fireplaces+Functional+BsmtUnfSF+ BsmtFinType1,
                   data = train1,
                   method = "gbm",metric = "RMSE",trControl = trainctrl, na.action=na.exclude)
gbmimppred = exp(predict(gbimpmodel,test1)) -1

gbmimpsubmission = data.frame(Id = test$Id,SalePrice = gbmimppred)
write.csv(gbmimpsubmission,"gbmimpsubmission.csv",row.names = F)


###############StepAIC######

stepmodel = step(lm(SalePrice ~ ., data = train1), direction = "both")
summary(stepmodel)
steppred = exp(predict(stepmodel,test1))-1
xgbsubmission =  data.frame(Id = test$Id,Saleprice = steppred)
write.csv(xgbsubmission,"stepsubmission.csv",row.names = F)

#step(lm(SalePrice ~ selvar, data = train1), direction = "both")

m1<-  lm(formula = SalePrice ~ `MSZoningC (all)` + MSZoningFV + MSZoningRH + 
           MSZoningRL + LotArea + AlleyGrvl + LandContourBnk + LandContourHLS + 
           LotConfigCulDSac + LandSlopeGtl + LandSlopeMod + Condition2 + 
           OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofMatlClyTile + 
           RoofMatlCompShg + RoofMatlRoll + `RoofMatlTar&Grv` + RoofMatlWdShake + 
           Exterior1st + Exterior2nd + MasVnrTypeBrkCmn + MasVnrTypeBrkFace + 
           MasVnrArea + ExterQual + ExterCond + FoundationPConc + FoundationSlab + 
           FoundationStone + BsmtQual + BsmtCond + BsmtFinSF1 + BsmtFinSF2 + 
           BsmtUnfSF + TotalBsmtSF + HeatingGrav + CentralAirN + ElectricalMix + 
           X1stFlrSF + LowQualFinSF + GrLivArea + BsmtFullBath + FullBath + 
           HalfBath + BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd + Functional + 
           Fireplaces + GarageType2Types + GarageTypeAttchd + GarageTypeBasment + 
           GarageTypeCarPort + GarageYrBlt + GarageCars + GarageArea + 
           GarageQualEx + WoodDeckSF + EnclosedPorch + ScreenPorch + 
           PoolArea + MiscFeatureGar2 + MiscFeatureShed + MiscVal + 
           SaleTypeCWD + SaleTypeNew + SaleConditionAbnorml + SaleConditionFamily, 
         data = train1)

############## SVM Model################

set.seed(12)
svmFit <-
  train(
    SalePrice ~ .,
    data = train1,
    method = "svmRadial",
    preProc = c("center", "scale"),
    tuneLength = 10,
    trControl = trainControl(method = "repeatedcv", repeats = 5),
    na.action = na.exclude
  )
svmFit$results
summary(svmFit)
plot(svmFit)
gridSearchCV(svmFit)

x <- subset(train1, select = -SalePrice)
y <- train1$SalePrice
pred <- predict(svmFit, x)
system.time(pred <- predict(svmFit, x))

svmfit$results

table(pred, y)
svm_tune <-
  tune(
    svm,
    train.x = x,
    train.y = y,
    kernel = "radial",
    ranges = list(cost = 10 ^ (-1:2), gamma = c(0.5, 1, 2))
  )

print(svm_tune)

svmFit1 <-
  svm(
    SalePrice ~ .,
    data = train1,
    kernel = "radial",
    cost = 1,
    gamma = 0.5,
    na.action = na.exclude
  )
summary(svmFit1)

pred <- predict(svmFit1, x)
system.time(pred <- predict(svmFit1, x))

best.tune(svmFit1.x)
print(svmFit1)

############## KNN Model################
library("class")
library("ggplot2")
library("lattice")
library("caret")

set.seed(12)
knnModel <- train(
  SalePrice ~ .,
  data = train1,
  method = "knn",
  metric = "RMSE",
  preProc = c('center', 'scale'),
  tuneLength = 10,
  trControl = trainctrl,
  na.action = na.exclude
)

knnModel$results
plot(knnModel)

knnmdpred = exp(predict(knnModel,test1))-1
knnsubmission =  data.frame(Id = test$Id,Saleprice = knnmdpred)
write.csv(knnsubmission,"KNNsubmission.csv",row.names = F)

######## Ridge Model ########
set.seed(1234)
ridge = train(SalePrice ~ .,train1,tuneGrid = expand.grid(alpha = 0,lambda = seq(0.0001,1,length = 10)),
              method = "glmnet",metric = "RMSE",trControl = trainctrl, na.action = na.exclude)
plot(ridge)
ridge
ridgepred = exp(predict(ridge,test1))-1
ridgesubmission =  data.frame(Id = test$Id,Saleprice = ridgepred)
write.csv(ridgesubmission,"RidgeSubmission.csv",row.names = F)
ridge$results
################Lasso MOdels#############0.1316###
set.seed(1234)
lasso = train(
  SalePrice ~ .,
  train1,
  tuneGrid = expand.grid(alpha = 1, lambda = seq(0.0001, 1, length = 10)),
  method = "glmnet",
  metric = "RMSE",
  trControl = trainctrl,
  na.action = na.exclude
)
lasso
lassopred = exp(predict(ridge,test1))-1
lassosubmission =  data.frame(Id = test$Id,Saleprice = lassopred)
write.csv(lassosubmission,"LassoSubmission.csv",row.names = F)
varImp(lasso)
plot(lasso)

################Elastic Net MOdels#############0.1316###
set.seed(1234)
en <-
  train(
    SalePrice ~ .,
    train1,
    method = 'glmnet',
    tuneGrid = expand.grid(
      alpha = seq(0, 1, length = 10),
      lambda = seq(0.0001, 1, length = 5)
    ),
    trControl = trainctrl,
    na.action = na.exclude
  )
plot(en)


####### Comparing The Results #########
lmcamodel$results


library("resample")
housing.results <- resamples(list(
  lm = lmpcamodel,
  lr = lmmodel,
  rf = rfmodel,
  knn = knnModel,
  ridge = ridge,
  lasso = lasso,
  enet = en
))

housing.results
#summary(housing.results)

############# MARS Model ###################
library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
library(xgboost)

housePriceDS <- read.csv(file.choose(),header = TRUE, stringsAsFactors = F)
train = read.csv(file.choose(),header = TRUE, stringsAsFactors = F)
test = read.csv(file.choose(),header = TRUE, stringsAsFactors = F)

dim(train)
dim(test)

test_labels <- test$Id
test$Id <- NULL
train$Id <- NULL
test$SalePrice <- NA
Housing <- rbind(train, test)

summary(Housing) #80 -> after removing unwanted columns 75 
#Removing Columns we are not interested in
Housing<- subset(Housing, select = -c(PoolQC,PoolArea,Fence,Alley,MiscFeature))

Housing_Num <- which(sapply(Housing, is.numeric)) #index vector numeric variables
Housing_Num_Names <- names(Housing_Num) #saving names vector for use later on
Housing_numVar <- Housing[, Housing_Num]
cor_numVar <- cor(Housing_numVar, use="pairwise.complete.obs")
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

# Handling missing data 
NA_Variables <- which(colSums(is.na(Housing)) > 0)
sort(colSums(sapply(Housing[NA_Variables], is.na)), decreasing = TRUE)

Housing$FireplaceQu[is.na(Housing$FireplaceQu)] <- 'None'
Housing$FireplaceQu<-as.integer(revalue(Housing$FireplaceQu, Qualities))
table(Housing$FireplaceQu)
#Imputing values
for (i in 1:nrow(Housing)){
  if(is.na(Housing$LotFrontage[i])){
    Housing$LotFrontage[i] <- as.integer(median(Housing$LotFrontage[Housing$Neighborhood==Housing$Neighborhood[i]], na.rm=TRUE)) 
  }
}
table(Housing$LotFrontage)
Housing$LotShape<-as.integer(revalue(Housing$LotShape, c('IR3'=0, 'IR2'=1, 'IR1'=2, 'Reg'=3)))
Housing$LotConfig <- as.factor(Housing$LotConfig)
table(Housing$LotConfig)

Housing$GarageYrBlt[is.na(Housing$GarageYrBlt)] <- Housing$YearBuilt[is.na(Housing$GarageYrBlt)]

kable(Housing[!is.na(Housing$GarageType) & is.na(Housing$GarageFinish), c('GarageCars', 'GarageArea', 'GarageType', 'GarageCond', 'GarageQual', 'GarageFinish')])
#Imputing by modes.
Housing$GarageCond[2127] <- names(sort(-table(Housing$GarageCond)))[1]
Housing$GarageQual[2127] <- names(sort(-table(Housing$GarageQual)))[1]
Housing$GarageFinish[2127] <- names(sort(-table(Housing$GarageFinish)))[1]
Housing$GarageCars[2577] <- 0
Housing$GarageArea[2577] <- 0
Housing$GarageType[2577] <- NA
Housing$GarageType[is.na(Housing$GarageType)] <- 'No Garage'
Housing$GarageType <- as.factor(Housing$GarageType)
table(Housing$GarageType)
Housing$GarageFinish[is.na(Housing$GarageFinish)] <- 'None'
Finish <- c('None'=0, 'Unf'=1, 'RFn'=2, 'Fin'=3)
Housing$GarageFinish<-as.integer(revalue(Housing$GarageFinish, Finish))
table(Housing$GarageFinish)
Qualities <- c('None'=0, 'Unf'=1, 'RFn'=2, 'Fin'=3)
Housing$GarageQual[is.na(Housing$GarageQual)] <- 'None'
Housing$GarageQual<-as.integer(revalue(Housing$GarageQual, Qualities))
table(Housing$GarageQual)
Housing$GarageCond[is.na(Housing$GarageCond)] <- 'None'
Housing$GarageCond<-as.integer(revalue(Housing$GarageCond, Qualities))
table(Housing$GarageCond)

#Imputing modes.
Housing$BsmtFinType2[333] <- names(sort(-table(Housing$BsmtFinType2)))[1]
Housing$BsmtExposure[c(949, 1488, 2349)] <- names(sort(-table(Housing$BsmtExposure)))[1]
Housing$BsmtCond[c(2041, 2186, 2525)] <- names(sort(-table(Housing$BsmtCond)))[1]
Housing$BsmtQual[c(2218, 2219)] <- names(sort(-table(Housing$BsmtQual)))[1]
Housing$BsmtQual[is.na(Housing$BsmtQual)] <- 'None'
Housing$BsmtQual<-as.integer(revalue(Housing$BsmtQual, Qualities))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 $BsmtQual<-as.integer(revalue(all$BsmtQual, Qualities))
table(Housing$BsmtQual)
Housing$BsmtCond[is.na(Housing$BsmtCond)] <- 'None'
Housing$BsmtCond<-as.integer(revalue(Housing$BsmtCond, Qualities))
table(Housing$BsmtCond)
Housing$BsmtExposure[is.na(Housing$BsmtExposure)] <- 'None'
Exposure <- c('None'=0, 'No'=1, 'Mn'=2, 'Av'=3, 'Gd'=4)
Housing$BsmtExposure<-as.integer(revalue(Housing$BsmtExposure, Exposure))
table(Housing$BsmtExposure)
Housing$BsmtFinType1[is.na(Housing$BsmtFinType1)] <- 'None'
FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
Housing$BsmtFinType1<-as.integer(revalue(Housing$BsmtFinType1, FinType))
table(Housing$BsmtFinType1)
Housing$BsmtFinType2[is.na(Housing$BsmtFinType2)] <- 'None'
FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
Housing$BsmtFinType2<-as.integer(revalue(Housing$BsmtFinType2, FinType))
table(Housing$BsmtFinType2)
Housing$BsmtFullBath[is.na(Housing$BsmtFullBath)] <-0
table(Housing$BsmtFullBath)
Housing$BsmtHalfBath[is.na(Housing$BsmtHalfBath)] <-0
table(Housing$BsmtHalfBath)
Housing$BsmtFinSF1[is.na(Housing$BsmtFinSF1)] <-0
Housing$BsmtFinSF2[is.na(Housing$BsmtFinSF2)] <-0
Housing$BsmtUnfSF[is.na(Housing$BsmtUnfSF)] <-0
Housing$TotalBsmtSF[is.na(Housing$TotalBsmtSF)] <-0

Housing$MasVnrType[2611] <- names(sort(-table(Housing$MasVnrType)))[2] #taking the 2nd value as the 1st is 'none'
Housing[2611, c('MasVnrType', 'MasVnrArea')]
Housing$MasVnrType[is.na(Housing$MasVnrType)] <- 'None'
Masonry <- c('None'=0, 'BrkCmn'=0, 'BrkFace'=1, 'Stone'=2)
Housing$MasVnrType<-as.integer(revalue(Housing$MasVnrType, Masonry))
table(Housing$MasVnrType)
Housing$MasVnrArea[is.na(Housing$MasVnrArea)] <-0

Housing$MSZoning[is.na(Housing$MSZoning)] <- names(sort(-table(Housing$MSZoning)))[1]
Housing$MSZoning <- as.factor(Housing$MSZoning)
table(Housing$MSZoning)

Housing$KitchenQual[is.na(Housing$KitchenQual)] <- 'TA' #replace with most common value
Housing$KitchenQual<-as.integer(revalue(Housing$KitchenQual, Qualities))
table(Housing$KitchenQual)
table(Housing$KitchenAbvGr)

table(Housing$Utilities)
kable(Housing[is.na(Housing$Utilities) | Housing$Utilities=='NoSeWa', 1:9])
Housing$Utilities <- NULL

Housing$Functional[is.na(Housing$Functional)] <- names(sort(-table(Housing$Functional)))[1]
Housing$Functional <- as.integer(revalue(Housing$Functional, c('Sal'=0, 'Sev'=1, 'Maj2'=2, 'Maj1'=3, 'Mod'=4, 'Min2'=5, 'Min1'=6, 'Typ'=7)))
table(Housing$Functional)

Housing$Exterior1st[is.na(Housing$Exterior1st)] <- names(sort(-table(Housing$Exterior1st)))[1]
Housing$Exterior1st <- as.factor(Housing$Exterior1st)
table(Housing$Exterior1st)
#imputing mode
Housing$Exterior2nd[is.na(Housing$Exterior2nd)] <- names(sort(-table(Housing$Exterior2nd)))[1]
Housing$Exterior2nd <- as.factor(Housing$Exterior2nd)
table(Housing$Exterior2nd)
Housing$ExterQual<-as.integer(revalue(Housing$ExterQual, Qualities))
table(Housing$ExterQual)
Housing$ExterCond<-as.integer(revalue(Housing$ExterCond, Qualities))
table(Housing$ExterCond)

#imputing mode
Housing$Electrical[is.na(Housing$Electrical)] <- names(sort(-table(Housing$Electrical)))[1]
Housing$Electrical <- as.factor(Housing$Electrical)
table(Housing$Electrical)

#imputing mode
Housing$SaleType[is.na(Housing$SaleType)] <- names(sort(-table(Housing$SaleType)))[1]
Housing$SaleType <- as.factor(Housing$SaleType)
table(Housing$SaleType)

Housing$SaleCondition <- as.factor(Housing$SaleCondition)
table(Housing$SaleCondition)

Charcol <- names(Housing[,sapply(Housing, is.character)])
Charcol

#No ordinality, so converting into factors
Housing$Foundation <- as.factor(Housing$Foundation)
table(Housing$Foundation)
Housing$Heating <- as.factor(Housing$Heating)
table(Housing$Heating)
Housing$RoofStyle <- as.factor(Housing$RoofStyle)
table(Housing$RoofStyle)
Housing$RoofMatl <- as.factor(Housing$RoofMatl)
table(Housing$RoofMatl)
Housing$LandContour <- as.factor(Housing$LandContour)
table(Housing$LandContour)
Housing$LandSlope<-as.integer(revalue(Housing$LandSlope, c('Sev'=0, 'Mod'=1, 'Gtl'=2)))
table(Housing$LandSlope)
Housing$BldgType <- as.factor(Housing$BldgType)
table(Housing$BldgType)
Housing$HouseStyle <- as.factor(Housing$HouseStyle)
table(Housing$HouseStyle)
Housing$Neighborhood <- as.factor(Housing$Neighborhood)
table(Housing$Neighborhood)
Housing$Condition1 <- as.factor(Housing$Condition1)
table(Housing$Condition1)
Housing$Condition2 <- as.factor(Housing$Condition2)
table(Housing$Condition2)
Housing$Street<-as.integer(revalue(Housing$Street, c('Grvl'=0, 'Pave'=1)))
table(Housing$Street)
Housing$PavedDrive<-as.integer(revalue(Housing$PavedDrive, c('N'=0, 'P'=1, 'Y'=2)))
table(Housing$PavedDrive)
# Month Sold converting to factor
Housing$MoSold <- as.factor(Housing$MoSold)
Housing$MSSubClass <- as.factor(Housing$MSSubClass)
Housing$MSSubClass<-revalue(Housing$MSSubClass, c('20'='1 story 1946+', '30'='1 story 1945-', '40'='1 story unf attic', '45'='1,5 story unf', '50'='1,5 story fin', '60'='2 story 1946+', '70'='2 story 1945-', '75'='2,5 story all ages', '80'='split/multi level', '85'='split foyer', '90'='duplex all style/age', '120'='1 story PUD 1946+', '150'='1,5 story PUD all', '160'='2 story PUD 1946+', '180'='PUD multilevel', '190'='2 family conversion'))
dim(Housing)

summary(Housing)
table(Housing$HeatingQC)
table(Housing$CentralAir)
sum(is.na(Housing))
na_count <-sapply(Housing, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

Housing$GarageYrBlt[2593] <- 2007

# Feature engineering
Housing$TotBathrooms <- Housing$FullBath + (Housing$HalfBath*0.5) + Housing$BsmtFullBath + (Housing$BsmtHalfBath*0.5)
Housing$Remod <- ifelse(Housing$YearBuilt==Housing$YearRemodAdd, 0, 1) #0=No Remodeling, 1=Remodeling
Housing$Age <- as.numeric(Housing$YrSold)-Housing$YearRemodAdd
cor(Housing$SalePrice[!is.na(Housing$SalePrice)], Housing$Age[!is.na(Housing$SalePrice)])
Housing$IsNew <- ifelse(Housing$YrSold==Housing$YearBuilt, 1, 0)
table(Housing$IsNew)

Housing$YrSold <- as.factor(Housing$YrSold)
Housing$NeighRich[Housing$Neighborhood %in% c('StoneBr', 'NridgHt', 'NoRidge')] <- 2
Housing$NeighRich[!Housing$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale', 'StoneBr', 'NridgHt', 'NoRidge')] <- 1
Housing$NeighRich[Housing$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale')] <- 0
table(Housing$NeighRich)

Housing$TotalSqFeet <- Housing$GrLivArea + Housing$TotalBsmtSF
cor(Housing$SalePrice, Housing$TotalSqFeet, use= "pairwise.complete.obs")
cor(Housing$SalePrice[-c(524, 1299)], Housing$TotalSqFeet[-c(524, 1299)], use= "pairwise.complete.obs")
Housing$TotalPorchSF <- Housing$OpenPorchSF + Housing$EnclosedPorch + Housing$X3SsnPorch + Housing$ScreenPorch
cor(Housing$SalePrice, Housing$TotalPorchSF, use= "pairwise.complete.obs")
dropVars <- c('YearRemodAdd', 'GarageYrBlt', 'GarageArea', 'GarageCond', 'TotalBsmtSF', 'TotalRmsAbvGrd', 'BsmtFinSF1')
Housing <- Housing[,!(names(Housing) %in% dropVars)]
Housing <- Housing[-c(524, 1299),]
numericVars <- which(sapply(Housing, is.numeric))
numericVarNames <- names(numericVars)
numericVarNames <- numericVarNames[!(numericVarNames %in% c('MSSubClass', 'MoSold', 'YrSold', 'SalePrice', 'OverallQual', 'OverallCond'))] #numericVarNames was created before having done anything
numericVarNames <- append(numericVarNames, c('Age', 'TotalPorchSF', 'TotBathrooms', 'TotalSqFeet'))

NVPredictor <- nearZeroVar(Housing)
Housing <- Housing[,-NVPredictor]

DFnumeric <- Housing[, names(Housing) %in% numericVarNames]

DFfactors <- Housing[, !(names(Housing) %in% numericVarNames)]
DFfactors <- DFfactors[, names(DFfactors) != 'SalePrice']

cat('There are', length(DFnumeric), 'numeric variables, and', length(DFfactors), 'factor variables')
## There are 30 numeric variables, and 49 factor variab
#There are 51 numeric variables, and 28 factor variables
for(i in 1:ncol(DFnumeric)){
  if (abs(skewness(DFnumeric[,i]))>0.8){
    DFnumeric[,i] <- log(DFnumeric[,i] +1)
  }
}
PreNum <- preProcess(DFnumeric, method=c("center", "scale"))
print(PreNum)
DFnorm <- predict(PreNum, DFnumeric)
dim(DFnorm)
DFdummies <- as.data.frame(model.matrix(~.-1, DFfactors))
dim(DFdummies)
ZerocolTest <- which(colSums(DFdummies[(nrow(Housing[!is.na(Housing$SalePrice),])+1):nrow(Housing),])==0)
colnames(DFdummies[ZerocolTest])
DFdummies <- DFdummies[,-ZerocolTest]
ZerocolTrain <- which(colSums(DFdummies[1:nrow(Housing[!is.na(Housing$SalePrice),]),])==0)
colnames(DFdummies[ZerocolTrain])
DFdummies <- DFdummies[,-ZerocolTrain] 
fewOnes <- which(colSums(DFdummies[1:nrow(Housing[!is.na(Housing$SalePrice),]),])<10)
colnames(DFdummies[fewOnes])
DFdummies <- DFdummies[,-fewOnes]
combined <- cbind(DFnorm, DFdummies)
skewness(Housing$SalePrice)
qqnorm(Housing$SalePrice)
qqline(Housing$SalePrice)
Housing$SalePrice <- log(Housing$SalePrice)

#Splitting data
train1 <- combined[!is.na(Housing$SalePrice),]
test1 <- combined[is.na(Housing$SalePrice),]

dim(train1)
dim(test1)

## MARS
library(earth)
library(plotmo)
set.seed(100)
length(Housing$SalePrice[!is.na(Housing$SalePrice)])
dim(train1)
marsFit <- earth(train1, Housing$SalePrice[!is.na(Housing$SalePrice)])
marsFit #Selected 28 of 31 terms, and 16 of 168 predictors
summary(marsFit)
plot(marsFit, which = 1)

marsGrid = expand.grid(.degree=1:3, .nprune=2:50)
marsModel = train(x=train1, y= Housing$SalePrice[!is.na(Housing$SalePrice)],
                  method="earth", preProc=c("center","scale"), tuneGrid=marsGrid, trControl = trainctrl)
summary(marsModel)

marsModel$bestTune #  nprune = 29      degree=1
plot(marsModel)

marsTrainPred = predict(marsModel, newdata=train1)
obsTrain <- Housing$SalePrice[!is.na(Housing$SalePrice)]
marsTrainPR = postResample(pred=marsTrainPred, obs=obsTrain)
marsTrainPR
#   RMSE  Rsquared       MAE 
#0.1114095 0.9222607 0.0768084

#variable Importance
plot(varImp(marsModel), top = 15, main="Variable Importance from MARS")

#test
marsTestPred = exp(predict(marsModel,test1))-1
head(marsTestPred)
MARSsubmission =  data.frame(Id = test$Id,Saleprice = marsTestPred)
write.csv(MARSsubmission,"MARSsubmission.csv",row.names = F)