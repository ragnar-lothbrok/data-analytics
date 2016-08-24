#Remove all env variables
rm(list = ls(all.names = T))
#Load the data from packages
install.packages("Ckmeans.1d.dp")
install.packages("DiagrammeR")
install.packages("corrplot")
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
library(IDPmisc)
library(Matrix)
library(xgboost)
library(readr)
library(car)
library("Ckmeans.1d.dp")
library(DiagrammeR)
library(corrplot)

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
#training = CTR_SD_Data[train_rows,c(-15,-11,-10,-4,-5,-8,-19)]
#training = CTR_SD_Data[train_rows,c(-15,-11,-10,-4,-5)]
training = CTR_SD_Data[train_rows,]
training[complete.cases(training), ]
NaRV.omit(training)

#Getting TEST data i.e. all rows not mentioned in train rows
testing = CTR_SD_Data[-train_rows,c(-15,-11,-10,-4,-5)]
testing[complete.cases(testing), ]
NaRV.omit(testing)

colnames(CTR_SD_Data)


dim(training)
training_mat <- Reduce(cbind2, lapply(training[,-2], Matrix, sparse = TRUE))
class(training_mat)[1]
str(training_mat)

bstSparse <- xgboost(data = data.matrix(training[,-2]), 
                     label = training$click,
                     booster = "gbtree", 
                     objective = "binary:logistic", 
                     max.depth = 5, 
                     eta = 0.5, 
                     nthread = 2, 
                     nround = 20, 
                     min_child_weight = 1, 
                     subsample = 0.5, 
                     colsample_bytree = 1, 
                     num_parallel_tree = 1)


pred <- predict(bstSparse, data.matrix(testing[,-1]))

# Get the feature real names
names <- dimnames(data.matrix(training[,-2]))[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = bstSparse)
importance_matrix
# Nice graph
xgb.plot.importance(importance_matrix[1:21,])
#In case last step does not work for you because of a version issue, you can try following :
barplot(importance_matrix[,1])

xgb.plot.tree(names, model = bstSparse)

View(round(cor(CTR_SD_Data),2))
corrplot(cor(training),method="number",sig.level = 0.01, insig = "blank")
