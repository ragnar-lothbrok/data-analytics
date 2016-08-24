#Remove all env variables
rm(list = ls(all.names = T))
#Load the data from packages
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
library(IDPmisc)
library(mlbench)

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

# calculate correlation matrix
correlationMatrix <- cor(CTR_SD_Data[,-3],use="pairwise.complete.obs")
NaRV.omit(correlationMatrix)
# summarize the correlation matrix
print(correlationMatrix)

# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)
colnames(CTR_SD_Data)


# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(CTR_SD_Data[,3:24], CTR_SD_Data[,2], sizes=c(3:24), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))







#Split the data in training and test data
rows = seq(from=1,to=nrow(CTR_SD_Data), by = 1)
train_rows = sample(x=rows, size=(0.7 * nrow(CTR_SD_Data))) #selecting 70% random sample no of row no as training data

#Getting training data i.e. selecting all rows that we had randomly selected from rows
training = CTR_SD_Data[train_rows,]
training[complete.cases(training), ]
NaRV.omit(training)

#Getting TEST data i.e. all rows not mentioned in train rows
testing = CTR_SD_Data[-train_rows,]
testing[complete.cases(testing), ]
NaRV.omit(testing)

param       = list("objective" = "multi:softmax", # multi class classification
                   "num_class"= 2 ,  		# Number of classes in the dependent variable.
                   "eval_metric" = "mlogloss",  	 # evaluation metric 
                   "nthread" = 8,   			 # number of threads to be used 
                   "max_depth" = 9,    		 # maximum depth of tree 
                   "eta" = 0.18,    			 # step size shrinkage 
                   "gamma" = 0,    			 # minimum loss reduction 
                   "subsample" = 0.7,    		 # part of data instances to grow tree 
                   "colsample_bytree" = 1, 		 # subsample ratio of columns when constructing each tree 
                   "min_child_weight" = 1		 # minimum sum of instance weight needed in a child 
)


predictors = colnames(training[-2])
#Alas, xgboost works only if the numeric labels
set.seed(100)

cv.nround = 500;  # Number of rounds. This can be set to a lower or higher value, if you wish, example: 150 or 250 or 300  
bst.cv = xgb.cv(
  param=param,
  data = as.matrix(training[,predictors]),
  label = training$click,
  nfold = 3,
  nrounds=cv.nround,
  missing='NAN',
  prediction=T)

min.loss.idx = which.min(bst.cv$dt[, test.mlogloss.mean]) 
cat ("Minimum logloss occurred in round : ", min.loss.idx, "\n")

# Minimum logloss
print(bst.cv$dt[min.loss.idx,])

# Step 2: Train the xgboost model using min.loss.idx found above.
#         Note, we have to stop at the round where we get the minumum error.
set.seed(100)

bst = xgboost(
  param=param,
  data =as.matrix(training[,predictors]),
  label = training$click,
  missing = 'NAN',
  nrounds=min.loss.idx)

na.omit(training[,predictors])

# Make prediction on the testing data.
prediction_click = predict(bst, as.matrix(training[,predictors]))

#Translate the prediction to the original class or Species.
prediction_click = ifelse(prediction_click == 0,0,1)

#Compute the accuracy of predictions.
confusionMatrix <- table( prediction_click ,training$click)

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

