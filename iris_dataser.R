#install.packages("ellipse")
#install.packages("ISLR")
library("caret")
library("ellipse")
library(ggplot2)
library(ISLR)

rm(list = ls(all.names = T)) #Clear all variables

#This will assign dataset to a variable
dataset <- iris

#This is used for viewing purpose
View(dataset)

#This will display columns present in dataset
colnames(dataset)

#Creating training data set
trainingDataSet <- createDataPartition(dataset$Species,p = 0.80,list = FALSE)

testDataSet <- dataset[-trainingDataSet,]

dataset <- dataset[trainingDataSet,]

#Overview of dataset rows and columns
dim(dataset)

#TO find out type of the fields.
sapply(dataset, class)

#Distinct value of factor type
levels(dataset$Species)

#This gives the group by
table(dataset$Species)

#Percentage of records for each field value
percentage <- prop.table(table(dataset$Species))

cbind(freq=table(dataset$Species), percentage=percentage * 100)

#This will print various stats like min/max/mean
summary(dataset)

x <- dataset[,1:4]
y <- dataset[,5]

par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}

# barplot for class breakdown
plot(y)


# scatterplot matrix
featurePlot(x=x, y=y, plot="ellipse")

# box and whisker plots for each attribute
featurePlot(x=x, y=y, plot="box")

featurePlot(x=x, y=y, plot="pairs")

# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# a) linear algorithms
set.seed(7)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)

# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

# compare accuracy of models
dotplot(results)

# summarize Best Model
print(fit.lda)

# estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, testDataSet)
?confusionMatrix(predictions, testDataSet$Species)
table(predictions)
