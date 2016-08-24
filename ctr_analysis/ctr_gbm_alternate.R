#Remove all env variables
rm(list = ls(all.names = T))
#Load the data from packages
library(caret)
library(pROC)


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


prop.table(table(CTR_SD_Data$click))

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


objControl <- trainControl(method='cv', number=3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)

objModel <- train(training, as.factor(training$click), 
                  method='gbm', 
                  trControl=objControl,  
                  metric = "ROC",
                  preProc = c("center", "scale"))
summary(objModel)
