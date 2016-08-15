#Remove all env variables
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
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(party)
library(randomForest)

CTR_SD_Data <- read.csv("/home/raghunandangupta/Downloads/splits/sub-splitaa")

#Vector to numeric converion Gives the datatype of each column
str(CTR_SD_Data)

table(CTR_SD_Data$click)
prop.table(table(CTR_SD_Data$click))

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

partiontioned_data <- createDataPartition(y=CTR_SD_Data$click,p = 0.4,list = FALSE)

training <- CTR_SD_Data[partiontioned_data,]
testing <- CTR_SD_Data[-partiontioned_data,]

#This will be used to get the group by
prop.table(table(training$click))

#Recursive Partitioning and Regression Trees
linear <- randomForest(training$click~., data = training)

summary(linear)

#Predict Output
predicted= predict(linear,testing)

predicted_class = factor(ifelse(test=predicted >0.5, yes = 1, no = 0))

length(predicted)
length(testing$click)

result <- table(testing$click,predicted_class)

error_p <- (result[2] + result[3]) / (result[1]+result[2]+result[3]+result[4]) * 100
print(error_p)

accuracy_p <- (result[1] + result[4]) / (result[1]+result[2]+result[3]+result[4]) * 100
print(accuracy_p)

#======================================================================================
#Keeping significant columns
training <- CTR_SD_Data[partiontioned_data,c("id","click","C1","banner_pos","site_id","site_domain","site_category","app_id","app_domain","app_category","device_model","device_type","device_conn_type","C14","C16","C17","C18","C19","C20","C21")]
testing <- CTR_SD_Data[-partiontioned_data,c("id","click","C1","banner_pos","site_id","site_domain","site_category","app_id","app_domain","app_category","device_model","device_type","device_conn_type","C14","C16","C17","C18","C19","C20","C21")]

#Used to remove na values
#na.omit(training)
#na.omit(testing)

linear <- randomForest(formula = training$click~id+C1+banner_pos+site_id+site_domain+site_category+app_id+app_domain+app_category+device_model+device_type+device_conn_type+C14+C16+C17+C18+C19+C20+C21, data = training)

#will provide basic details about model F-statistics of the significance test with the summary function
summary(linear)

#Predict Output
predicted= predict(linear,testing)

predicted_class = factor(ifelse(test=predicted >0.5, yes = 1, no = 0))

length(predicted)
length(testing$click)

result <- table(testing$click,predicted_class)

error_p <- (result[2] + result[3]) / (result[1]+result[2]+result[3]+result[4]) * 100
print(error_p)

accuracy_p <- (result[1] + result[4]) / (result[1]+result[2]+result[3]+result[4]) * 100
print(accuracy_p)