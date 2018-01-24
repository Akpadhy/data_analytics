############################ SVM Letter Recogniser #################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
#  4.1 Linear kernel
#  4.2 RBF Kernel
# 5 Hyperparameter tuning and cross validation

#####################################################################################

# 1. Business Understanding: 

#The objective is to identify each of a large number of black-and-white
#rectangular pixel displays as one of the 9 digits

#####################################################################################

# 2. Data Understanding: 
# https://archive.ics.uci.edu/ml/datasets/letter+recognition

#3. Data Preparation: 


#Loading Neccessary libraries

library(kernlab)
library(readr)
library(caret)


#Loading Data
setwd("~/Downloads")
train<-read.csv("mnist_train.csv",header=F)
test<-read.csv("mnist_test.csv",header=F)

#Understanding Dimensions

dim(train)

#Structure of the dataset

str(train)

#printing first few rows

head(train)

#Exploring the data

summary(train)

#checking missing value

sapply(train, function(x) sum(is.na(x)))


#Making our target class to factor

# Split the data into train and test set

set.seed(100)
library(dplyr)
train_sample<-sample_n(train, 2000, replace = FALSE, weight = NULL, .env = NULL)
train_sample$V1

train_sample$V1 <- as.factor(train_sample$V1)


#Constructing Model

#Using Linear Kernel
Model_linear <- ksvm(V1~ ., data = train_sample, scale = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, test)
print(Model_linear)
#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,test$V1)


#Using RBF Kernel
Model_RBF <- ksvm(V1~ ., data = train_sample, scale = FALSE, kernel = "rbfdot")
Eval_RBF<- predict(Model_RBF, test)
print(Model_RBF)
#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,test$V1)


############   Hyperparameter tuning and Cross Validation #####################

# We will use the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 2 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=3)


# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.

metric <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.

set.seed(7)

#cross validation
#linear
grid <- expand.grid(C=seq(1, 5, by=1))
fit.svm <- train(V1~., data=train_sample, method="svmLinear", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)
print(fit.svm)
plot(fit.svm)

#RBF
grid <- expand.grid(.sigma=1.65179779760302e-07, .C=c(2,5,6,7,8) )

fit.svm.rbf <- train(V1~., data=train_sample, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

print(fit.svm.rbf)

plot(fit.svm.rbf)
