# MIS581
Pima Indians Diabetes Data Set Classification Tree

Sys.time()
#Load data mining packages
library(rpart)
library(rpart.plot)
library(caret)
#Required to get data but I don't know why it's needed to load the data
if(!require('mlbench')) {
  install.packages('mlbench')
  library('mlbench')
}
#Get cleaned data
#PimaIndiansDiabetes2 removes zeros that were not possible
##Replaced with NA
data(PimaIndiansDiabetes2) #Cleaned data set from R-Project.org
summary(PimaIndiansDiabetes2) #Summary of data set

#Create a data frame
Diabetes.df <- PimaIndiansDiabetes2
Diabetesdf2 <- na.omit(Diabetes.df) #Remove NAs to fix issues with models
head(Diabetesdf2)
summary(Diabetesdf2)
summary(Diabetesdf2$diabetes) #Number of participants without/with diabetes
Sys.Date()

#Create Correlation Matrix 
install.packages("corrplot")
library(corrplot)
install.packages("Hmisc")
library("Hmisc")
Sys.Date()
Diabetes.cor <- round(cor(Diabetesdf2[1:8]),1)
Diabetes.cor

#Partition Preparation
set.seed(1) #To reproduce the same output of simulation studies

#Partition Data Set
train.index <- sample(c(1:dim(Diabetesdf2)[1]),
                      dim(Diabetesdf2)[1]*0.6) #Sample data frame
test.index <- sample(c(1:dim(Diabetesdf2)[1]),
                     dim(Diabetes.df)[1]*0.4) #Sample data frame
### New Partition
require(caTools)
set.seed(3)
sample = sample.split(Diabetesdf2$diabetes, SplitRatio = 0.75)
train= subset(Diabetesdf2, sample==TRUE)
test= subset(Diabetesdf2, sample== FALSE)

#Training Partition
train.df <- Diabetesdf2[train.index,]
#Testing Partition
test.df <- Diabetesdf2[test.index,]

#Create Logistic Model for Diabetes with Training Data
Train.Model <- glm(train$diabetes ~ ., 
                family = binomial(link = "logit"), data = train)
Sys.Date()
summary(Train.Model)
plot(Train.Model)

#Create Logistic Model for Diabetes with Test.df
Sys.Date()
Test.model <- glm(test$diabetes ~ .,
                 family = binomial(link = "logit"), data = test)
summary(Test.model)
View(Test.model)
#Graph of Logistic Model using Test Partition
library(ggplot2)
plot(Test.Model)


#Prediction statistics
Train.pred <- predict(Train.Model, type = "link")
Sys.Date()
summary(Train.pred)
Test.pred <- predict(Test.Model, type = "link")
summary(Test.pred)

#ROC Curve###WOULDN'T WORK
install.packages("ROCR")
library("ROCR")
Train.ROCR = prediction(train$diabetes, Diabetesdf2)
Train.perf = performance(Train.ROCR, "tpr", "fpr")
#Confusion matrix
table(test$diabetes, Test.pred > 0.5)

#####Classification Tree 
library(rpart)
library(rpart.plot)
##Tree using Train Data
Diabetes.Tree.Train <- rpart(train$diabetes ~., data = train,
                             method = "class", minbucket = 10)
prp(Diabetes.Tree.Train)##view tree
##Tree using Test Data
Diabetes.Tree <- rpart(test$diabetes ~ ., data = test,
                        method = "class", minbucket = 5)
Diabetes.Tree
prp(Diabetes.Tree)## View tree

#Evaluate Tree 
Train.Tree.pred <- predict(Diabetes.Tree.Train, newdata = train,
                           type = "class")
table(Train.Tree.pred)

Sys.Date()
Test.Tree.pred <- predict(Diabetes.Tree, newdata = test,
                          type = "class")

table(Test.Tree.pred)
Sys.Date()
table(train$diabetes, Train.Tree.pred)
sensitivity.Train <- round(72/(26+72),2)
specificity.Train <- round(178/(178+18),2)
sprintf("Sensitivity at 0.7 threshold: %s", sensitivity.Train)                          
sprintf("Specificity at 0.7 threshold: %s", specificity.Train)                          

table(test$diabetes, Test.Tree.pred)
sensitivity.Test <- round(24/(24+8),2)
specificity.Test <- round(59/(59+7),2)
sprintf("Sensitivity at 0.7 threshold: %s", sensitivity.Test)                          
sprintf("Specificity at 0.7 threshold: %s", specificity.Test)
