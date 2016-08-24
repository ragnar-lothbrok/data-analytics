#Remove all env variables
rm(list = ls(all.names = T))
#Load the data from packages
library(pROC)
library(outliers)
library(fmsb)
library(ResourceSelection)
library(e1071)
detach("package:usdm", unload=TRUE)
library(car)
library(lattice)
library(ggplot2)
library(caret)

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

partiontioned_data <- createDataPartition(y=CTR_SD_Data$click,p = 0.7,list = FALSE)

training <- CTR_SD_Data[partiontioned_data,]
testing <- CTR_SD_Data[-partiontioned_data,]

linear <- lm(training$click~., data = training)

#will provide basic details about model F-statistics of the significance test with the summary function
summary(linear)

#Predict Output
predicted= predict(linear,testing)

predicted_class = factor(ifelse(test=predicted > 0.5, yes = 1, no = 0))

confusionMatrix <- table(testing$click,predicted_class)
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

#======================================================================================
#Keeping significant columns
training <- CTR_SD_Data[partiontioned_data,c("id","click","C1","banner_pos","site_id","site_domain","site_category","app_id","app_domain","app_category","device_model","device_type","device_conn_type","C14","C16","C17","C18","C19","C20","C21")]
testing <- CTR_SD_Data[-partiontioned_data,c("id","click","C1","banner_pos","site_id","site_domain","site_category","app_id","app_domain","app_category","device_model","device_type","device_conn_type","C14","C16","C17","C18","C19","C20","C21")]

#Used to remove na values
#na.omit(training)
#na.omit(testing)

linear <- lm(formula = training$click~id+C1+banner_pos+site_id+site_domain+site_category+app_id+app_domain+app_category+device_model+device_type+device_conn_type+C14+C16+C17+C18+C19+C20+C21, data = training,na.action = na.exclude)

#will provide basic details about model F-statistics of the significance test with the summary function
summary(linear)

#This will print coefficient values
coeffs = coefficients(linear);
print(coeffs)

#Predict Output
predicted= predict(linear,testing)

predicted_class = factor(ifelse(test=predicted > 0.5, yes = 1, no = 0))

#This is used for avoiding all arguments must have the same length
length(predicted_class)
length(testing$click)

confusionMatrix <- table(testing$click,predicted_class)
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

