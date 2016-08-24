#Remove all env variables
install.packages("klaR")
rm(list = ls(all.names = T))
#Load the data from packages
library(pROC)
library(outliers)
library(fmsb)
library(ResourceSelection)
library(e1071)
setRepositories()
detach("package:usdm", unload=TRUE)
library(car)
library(lattice)
library(ggplot2)
library(caret)
library("klaR")

CTR_SD_Data <- read.csv("/home/raghunandangupta/Downloads/splits/sub-testtaa")
#Vector to numeric converion Gives the datatype of each column
str(CTR_SD_Data)

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
head(rows)
train_rows = sample(x=rows, size=(0.7 * nrow(CTR_SD_Data))) #selecting 70% random sample no of row no as training data
head(train_rows)

#Getting training data i.e. selecting all rows that we had randomly selected from rows
TrainData = CTR_SD_Data[train_rows,]
#Getting TEST data i.e. all rows not mentioned in train rows
TestData = CTR_SD_Data[-train_rows,]

#Building model on train data
nblassifier<-naiveBayes(TrainData[,-2], factor(TrainData[,2]))

#Calculate accuracy, precision, recall
confusionMatrix = table(predict(nblassifier, TestData[,-2]), factor(TestData[,2]));
confusionMatrix

# TP + TN / FP + FN
accuracry = sum(diag(confusionMatrix))/sum(confusionMatrix) * 100;
accuracry

#Precision : proportion of predicted positive test cases which is true TP / (TP+FP)
precision = confusionMatrix[2,2] / (confusionMatrix[2,2] + confusionMatrix[1,2]);
precision

# Sensitivity Recall : proportion of predicted positive test cases / actual postive test cases TP / (TP + FN) or true positive rate
recall = confusionMatrix[2,2] / (confusionMatrix[2,2] + confusionMatrix[2,1]);
recall

#Spcecificity TN / (TN + FP)
s <- confusionMatrix[1,1] / (confusionMatrix[1,2] + confusionMatrix[1,2])
s

#False positive rate : predicted +ve said amongst actual negative test case
fpr = confusionMatrix[1,2] / (confusionMatrix[1,1] + confusionMatrix[1,2]);
fpr

#F = 2PR / P + R
f <- (2*(confusionMatrix[2,2] / (confusionMatrix[2,2] + confusionMatrix[2,1]))*(confusionMatrix[2,2] / (confusionMatrix[2,2] + confusionMatrix[1,2]))) / ((confusionMatrix[2,2] / (confusionMatrix[2,2] + confusionMatrix[2,1]))+(confusionMatrix[2,2] / (confusionMatrix[2,2] + confusionMatrix[1,2])))
f

#======================================================================================