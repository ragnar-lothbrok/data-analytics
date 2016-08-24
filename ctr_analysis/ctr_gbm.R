#Remove all env variables
rm(list = ls(all.names = T))
#Load the data from packages
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
library(IDPmisc)
library(plyr)
library(parallel)
library(splines)
library(gbm)
library(caTools)


set.seed(100)
CTR_SD_Data <- read.csv("/home/raghunandangupta/Downloads/splits/sub-testtaa")

#Convert categorical values to numeric
CTR_SD_Data$site_id             = as.numeric(CTR_SD_Data$site_id)
CTR_SD_Data$site_domain         = as.numeric(CTR_SD_Data$site_domain)
CTR_SD_Data$site_category       = as.numeric(CTR_SD_Data$site_category)
CTR_SD_Data$app_id              = as.numeric(CTR_SD_Data$app_id)
CTR_SD_Data$app_domain          = as.numeric(CTR_SD_Data$app_domain)
CTR_SD_Data$app_category        = as.numeric(CTR_SD_Data$app_category)
CTR_SD_Data$device_id           = as.numeric(CTR_SD_Data$device_id)
CTR_SD_Data$device_ip           = as.numeric(CTR_SD_Data$device_ip)
CTR_SD_Data$device_model        = as.numeric(CTR_SD_Data$device_model)

#Split the data in training and test data
rows = seq(from=1,to=nrow(CTR_SD_Data), by = 1)
train_rows = sample(x=rows, size=(0.7 * nrow(CTR_SD_Data))) #selecting 70% random sample no of row no as training data

#Getting training data i.e. selecting all rows that we had randomly selected from rows
training = CTR_SD_Data[train_rows,-3]
training[complete.cases(training), ]
NaRV.omit(training)

#Getting TEST data i.e. all rows not mentioned in train rows
testing = CTR_SD_Data[-train_rows,-3]
testing[complete.cases(testing), ]
NaRV.omit(testing)
set.seed(33)

gbm = gbm(training$click~., training,
          n.trees=50,
          shrinkage=0.01,
          distribution="gaussian",
          #distribution="bernoulli",
          interaction.depth=7,
          bag.fraction=0.9,
          cv.fold=10,
          n.minobsinnode = 50
          )

summary(gbm)
best.iter <- gbm.perf(gbm,method="cv")

train_pred <- predict.gbm(gbm,training,best.iter)
test_pred <- predict.gbm(gbm,training,best.iter)
