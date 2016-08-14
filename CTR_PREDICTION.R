#Remove all env variables
rm(list = ls(all.names = T)) #Clear all variables

#Trying to recompile to remove error of package class was built before 3.0.0
install.packages('codetools')
update.packages(checkBuilt = TRUE, ask = FALSE)
install.packages("shiny")

#Install required packages
install.packages("fmsb") #VIF
install.packages("DMwR") #Data Mining
install.packages("outliers")
install.packages("pROC") #Receiver operating curve
install.packages("ROCR")
install.packages("aod") #Wald-test
install.packages("ResourceSelection") #hosmer-lemeshow test
install.packages("caret")
install.packages("XLConnect")
install.packages("pbkrtest")
install.packages('e1071', dependencies=TRUE) #Naive Bayes and Also for SVM
install.packages("pROC")
install.packages('car', dependencies = TRUE) 

#Load the data from packages
library(pROC)
library(outliers)
library(fmsb)
library(ResourceSelection)
#library(vif)
library(e1071)

#To solve the error of vif
setRepositories()
detach("package:usdm", unload=TRUE)
library(car)
#??vif

CTR_SD_Data <- read.csv("/home/raghunandangupta/Downloads/splits/aa")
View(CTR_SD_Data)

#Vector to numeric converion
str(CTR_SD_Data)  #Gives the datatype of each column
CTR_SD_Data$site_id = as.numeric(CTR_SD_Data$site_id)
str(CTR_SD_Data)
CTR_SD_Data$site_domain = as.numeric(CTR_SD_Data$site_domain)
CTR_SD_Data$site_category = as.numeric(CTR_SD_Data$site_category)
CTR_SD_Data$app_id = as.numeric(CTR_SD_Data$app_id)
CTR_SD_Data$app_domain = as.numeric(CTR_SD_Data$app_domain)
CTR_SD_Data$app_category = as.numeric(CTR_SD_Data$app_category)
CTR_SD_Data$device_id = as.numeric(CTR_SD_Data$device_id)
CTR_SD_Data$device_ip = as.numeric(CTR_SD_Data$device_ip)
CTR_SD_Data$device_model = as.numeric(CTR_SD_Data$device_model)
CTR_SD_Data$site_id=as.numeric(CTR_SD_Data$site_id)
str(CTR_SD_Data)

#Split the data in training and test data
rows = seq(from=1,to=nrow(CTR_SD_Data), by = 1)
rows
train_rows = sample(x=rows, size=(0.7 * nrow(CTR_SD_Data))) #selecting 70% random sample no of row no as training data
train_rows
TrainData = CTR_SD_Data[train_rows,] #Getting training data i.e. selecting all rows that we had randomly selected from rows
TestData = CTR_SD_Data[-train_rows,] #Getting TEST data i.e. all rows not mentioned in train rows


str(TrainData)
#Check for ccorelation and multicollinearity
#multicollinearity_matrix = cor(TrainData[-1])
multicollinearity_matrix = cor(TrainData)#Discarding 1st column while checking ulticollinearitty
View(multicollinearity_matrix)
write.csv(multicollinearity_matrix, file="/home/raghunandangupta/Downloads/splits/sub-splitaaaa" )

vif = vif(TrainData[-2]) #calculates vif
vifcor(TrainData[-1], th=0.8) #Drops one of the column which is corelated

#Building Logistic Regression
log_reg_2 = glm(formula = click~., family = binomial, data=TrainData) #glm = generlaized linera model
log_reg_2
summary(log_reg_2) #AIC is error

#######################################################
#Prediction on train data

train_set_prob = predict(object = log_reg_2, newdata = TrainData[,-2], type="response")
head(train_set_prob)
table(train_set_prob)

#Prediction on test data

test_set_prob = predict(object = log_reg_2, newdata = TestData[,-2], type="response")
?predict
head(test_set_prob)  #Gives proability. logistic regression gives probability as output

#Now we have to decide saying, say if the probability is > 0.5, then we consider default variable as 1 i.e.
test_pred_class = factor(ifelse(test=test_set_prob >0.1, yes = 1, no = 0))
names(test_pred_class)

table(TestData$click, test_pred_class)
model_accuracy = (41485+58)/(41485+103+8888+58); model_accuracy

#Hosmer -Lemshow goodness of fit (GOF) Test : Nulll hypothesis is good fit
hoslem.test(TrainData$Default, fitted(log_reg_2), g=10) #Value being < 0.05, the GOF is not fit for model
hoslem.test(TestData$Default, y=test_set_prob, g=10) #Value being < 0.05, the GOF is not fit for model


#Calculate accuracy, precision, recall
confusionMatrix = table(TestData$click, test_pred_class); confusionMatrix
accuracry = sum(diag(confusionMatrix))/sum(confusionMatrix) * 100; accuracry

#Precision : proportion of predicted positive test cases which is true
precision = confusionMatrix[2,2] / (confusionMatrix[2,2] + confusionMatrix[1,2]); precision

#Recall : proportion of predicted positive test cases / actual postive test cases
recall = confusionMatrix[2,2] / (confusionMatrix[2,2] + confusionMatrix[2,1]); recall

#False positive rate : predicted +ve said amongst actual negative test case
fpr = confusionMatrix[1,2] / (confusionMatrix[1,1] + confusionMatrix[1,2]); fpr

plot(TrainData$click,TrainData$device_id)
