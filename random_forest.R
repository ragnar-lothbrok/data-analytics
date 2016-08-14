#RandomForest
install.packages("h2o")
install.packages("randomForest")
library(randomForest)
CTR_SD_Data <- read.csv("/home/raghunandangupta/Downloads/splits/splitaa")

rows = seq(from=1,to=nrow(CTR_SD_Data), by = 1)
head(rows)
train_rows = sample(x=rows, size=(0.7 * nrow(CTR_SD_Data))) #selecting 70% random sample no of row no as training data
head(train_rows)
TrainData = CTR_SD_Data[train_rows,] #Getting training data i.e. selecting all rows that we had randomly selected from rows
TestData = CTR_SD_Data[-train_rows,] #Getting TEST data i.e. all rows not mentioned in train rows


rfClassifier <- randomForest(click ~ ., data = TrainData)


#Calculate accuracy, precision, recall
confusionMatrix = table(predict(rfClassifier, TestData[,-2]), factor(TestData[,2])); confusionMatrix
accuracry = sum(diag(confusionMatrix))/sum(confusionMatrix) * 100; accuracry

#Precision : proportion of predicted positive test cases which is true
precision = confusionMatrix[2,2] / (confusionMatrix[2,2] + confusionMatrix[1,2]); precision

#Recall : proportion of predicted positive test cases / actual postive test cases
recall = confusionMatrix[2,2] / (confusionMatrix[2,2] + confusionMatrix[2,1]); recall

#False positive rate : predicted +ve said amongst actual negative test case
fpr = confusionMatrix[1,2] / (confusionMatrix[1,1] + confusionMatrix[1,2])
