#Remove all env variables
rm(list = ls(all.names = T))
#Load the data from packages
library(pROC)
library(ResourceSelection)
library(e1071)
detach("package:usdm", unload=TRUE)
library(caret)

CTR_SD_Data <- read.csv("/home/raghunandangupta/Downloads/soc_gen_data/train.csv")
cols <- colnames(CTR_SD_Data)
CTR_SD_Data[,-102] = log(CTR_SD_Data[,-102])
CTR_SD_Data$Y = ifelse(test=CTR_SD_Data$Y  == -1, no = 1, yes = 0)
head(CTR_SD_Data[,])
#Vector to numeric converion Gives the datatype of each column
str(CTR_SD_Data)
set.seed(4)
CTR_SD_Data <- CTR_SD_Data[sample(nrow(CTR_SD_Data)),]


partiontioned_data <- createDataPartition(y=CTR_SD_Data$Y,p = 0.7,list = FALSE)

training <- CTR_SD_Data[partiontioned_data,]
head(training)
#testing <- CTR_SD_Data[-partiontioned_data,]
nrow(training)

TEST_DATA <- read.csv("/home/raghunandangupta/Downloads/soc_gen_data/test.csv")
TEST_DATA = log(TEST_DATA)
testing <- TEST_DATA
nrow(testing)
testing

linear <- lm(training$Y~., data = training)

#will provide basic details about model F-statistics of the significance test with the summary function
summary(linear)

#Predict Output
predicted= predict(linear,testing)

predicted_class <-  factor(ifelse(test=predicted > 0.5, yes = 1, no = -1))
#predicted_class <-  factor(ifelse(test=predicted > 0.4, yes = 1, no = -1))
#predicted_class <-  factor(ifelse(test=predicted > 0.4, yes = -1, no = 1))

length(predicted_class)


result <- data.frame(predicted_class)
table(result)

head(result,361)
result
